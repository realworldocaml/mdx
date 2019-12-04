open Bos
open Stdune

type cache = { cache_dir : Fpath.t option }

let no_cache = { cache_dir = None }

let get_env_path var =
  let open Option.O in
  Sys.getenv_opt var >>| fun str ->
  Result.map_error
    ~f:(function `Msg s -> `Msg (Printf.sprintf "$%s: %s" var s))
    (Fpath.of_string str)

let get_cache_dir () =
  let open Option.O in
  let duniverse_cache () = get_env_path "DUNIVERSE_CACHE" in
  let xdg_cache () = get_env_path "XDG_CACHE_HOME" in
  let home_cache () =
    get_env_path "HOME" >>| fun home ->
    let open Result.O in
    home >>| fun home ->
    if Sys.win32 then Fpath.(home / "Local Settings" / "Cache") else Fpath.(home / ".cache")
  in
  let win_app_data_cache () =
    if Sys.win32 then
      get_env_path "AppData" >>| fun app_data ->
      let open Result.O in
      app_data >>| fun app_data -> Fpath.(app_data / "Local Settings" / "Cache")
    else None
  in
  List.find_map
    ~f:(fun get -> get ())
    [ duniverse_cache; xdg_cache; home_cache; win_app_data_cache ]

let get_cache () =
  match get_cache_dir () with
  | None ->
      Logs.warn (fun l ->
          l
            "Coudln't find a cache directory on your system. None of $HOME, $XDG_CACHE_HOME or \
             $DUNIVERSE_CACHE was set. Duniverse cache is disabled for now.");
      Ok { cache_dir = None }
  | Some path ->
      let open Result.O in
      path >>= fun path ->
      let cache_dir = Fpath.(path / "duniverse") in
      Bos.OS.Dir.create ~path:true ~mode:0o700 cache_dir >>= fun _created ->
      Ok { cache_dir = Some cache_dir }

module Cached = struct
  let valid_remote_name = Astring.String.map (function '/' | '@' | ':' -> '-' | x -> x)

  let commit_branch_name ~commit = "commit-" ^ commit

  let cache_branch_name ~ref = "branch-" ^ ref

  (** Check if the cache repository is initialized and if not, initialize it *)
  let check_duniverse_cache_directory ~repo ~remote =
    let open Result.O in
    let ok_file = Fpath.(repo / "duniverse_OK") in
    OS.Path.exists ok_file >>= function
    | true -> Ok ()
    | false ->
        OS.Dir.delete ~recurse:true repo >>= fun () ->
        OS.Dir.create repo >>= fun _ ->
        Exec.git_init_bare ~repo >>= fun () ->
        Exec.git_remote_add ~repo ~remote_url:remote ~remote_name:"origin" >>= fun () ->
        OS.File.write ok_file ""

  (** Get the path for the given remote in the cache *)
  let get_cache_directory ~remote cache_dir =
    let open Result.O in
    let remote_dir = valid_remote_name remote in
    let repo = Fpath.(cache_dir / remote_dir) in
    check_duniverse_cache_directory ~repo ~remote >>| fun () -> repo

  let git_branch_exists_or_create ~repo ~ref ~branch =
    let open Result.O in
    if Exec.git_branch_exists ~repo ~branch then Ok true
    else Exec.git_fetch_to ~repo ~remote_name:"origin" ~ref ~branch () >>| fun () -> false

  let git_commit_branch_exists_or_create ~repo ~ref ~branch ~commit ~commit_branch () =
    let open Result.O in
    if Exec.git_branch_exists ~repo ~branch:commit_branch then Ok true
    else
      git_branch_exists_or_create ~repo ~ref ~branch >>= fun cached ->
      Exec.git_branch ~repo ~ref:commit ~branch_name:commit_branch >>| fun () -> cached

  (** Check if a remote with a given tag exists in the cache as a branch, and clone to cache if it
      doesn't exist *)
  let check_package_cache_branch ~repo ~ref ~commit () =
    let open Result.O in
    let commit_branch = commit_branch_name ~commit in
    let branch = cache_branch_name ~ref in
    git_commit_branch_exists_or_create ~repo ~ref ~branch ~commit ~commit_branch ()
    >>| fun cached -> (commit_branch, cached)

  (** Clone to output_dir using the cache *)
  let clone_from_cache ~output_dir ~repo (cache_branch_name, cached) =
    let open Result.O in
    OS.Dir.delete ~recurse:true output_dir >>= fun () ->
    Exec.git_clone ~branch:cache_branch_name ~remote:(Fpath.to_string repo) ~output_dir
    >>| fun () -> cached

  let clone_to ~output_dir ~remote ~ref ~commit cache_dir =
    let open Result.O in
    get_cache_directory ~remote cache_dir >>= fun repo ->
    check_package_cache_branch ~repo ~ref ~commit () >>= clone_from_cache ~output_dir ~repo
end

module Uncached = struct
  let warn_about_head_commit ~ref ~commit () =
    Logs.info (fun l ->
        l "%a is not the HEAD commit for %a anymore" Styled_pp.commit commit Styled_pp.branch ref);
    Logs.info (fun l -> l "You might want to consider running 'duniverse update'");
    ()

  let checkout_if_needed ~head_commit ~output_dir ~ref ~commit () =
    let open Result.O in
    if String.equal commit head_commit then Ok ()
    else (
      warn_about_head_commit ~ref ~commit ();
      Exec.git_unshallow ~repo:output_dir () >>= fun () -> Exec.git_checkout ~repo:output_dir commit
      )

  let clone_to ~output_dir ~remote ~ref ~commit () =
    let open Result.O in
    Exec.git_shallow_clone ~output_dir ~remote ~ref () >>= fun () ->
    Exec.git_rev_parse ~repo:output_dir ~ref:"HEAD" () >>= fun head_commit ->
    checkout_if_needed ~head_commit ~output_dir ~ref ~commit () >>= fun () -> Ok false
end

let clone_to ~output_dir ~remote ~ref ~commit { cache_dir } =
  match cache_dir with
  | None -> Uncached.clone_to ~output_dir ~remote ~ref ~commit ()
  | Some cache_dir -> Cached.clone_to ~output_dir ~remote ~ref ~commit cache_dir
