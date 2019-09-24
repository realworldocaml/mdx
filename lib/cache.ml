open Bos
open Rresult
open Astring

let valid_remote_name = String.map (function '/' | '@' | ':' -> '-' | x -> x)

let commit_branch_name ~commit = "commit-" ^ commit

let cache_branch_name ~ref = "branch-" ^ ref

let home () =
  try Sys.getenv "HOME"
  with Not_found -> (
    try (Unix.getpwuid (Unix.getuid ())).Unix.pw_dir
    with Unix.Unix_error _ | Not_found ->
      if Sys.win32 then try Sys.getenv "AppData" with Not_found -> "" else "" )

(** Check if the cache repository is initialized and if not, initialize it *)
let check_duniverse_cache_directory ~repo ~remote =
  let ok_file = Fpath.(repo / "duniverse_OK") in
  OS.Path.exists ok_file >>= function
  | true -> Ok ()
  | false ->
      OS.Dir.delete ~recurse:true repo >>= fun () ->
      OS.Dir.create repo >>= fun _ ->
      Exec.git_init_bare ~repo >>= fun () ->
      Exec.git_remote_add ~repo ~remote_url:remote ~remote_name:"origin" >>= fun () ->
      OS.File.write ok_file ""

(** Get the location of the duniverse cache repository + create directory if it doesn't exist. *)
let get_cache_directory ~remote () =
  let remote_dir = valid_remote_name remote in
  let ( / ) = Filename.concat in
  let default_cache_dir () =
    let os_cache_dir = if Sys.win32 then "Local Settings" / "Cache" else ".cache" in
    home () / os_cache_dir
  in
  let xdg_cache_dir () =
    match Bos.OS.Env.var "XDG_CACHE_HOME" with Some x -> x | None -> default_cache_dir ()
  in
  let cache_dir () =
    match Bos.OS.Env.var "DUNIVERSE_CACHE" with Some x -> x | None -> xdg_cache_dir ()
  in
  Fpath.of_string (cache_dir () / "duniverse" / remote_dir)
  |> Rresult.R.reword_error (function `Msg oui -> `Msg oui)
  >>= fun repo -> check_duniverse_cache_directory ~repo ~remote >>| fun () -> repo

let git_branch_exists_or_create ~repo ~ref ~branch =
  if Exec.git_branch_exists ~repo ~branch then Ok true
  else Exec.git_fetch_to ~repo ~remote_name:"origin" ~ref ~branch () >>| fun () -> false

let git_commit_branch_exists_or_create ~repo ~ref ~branch ~commit ~commit_branch () =
  if Exec.git_branch_exists ~repo ~branch:commit_branch then Ok true
  else
    git_branch_exists_or_create ~repo ~ref ~branch >>= fun cached ->
    Exec.git_branch ~repo ~ref:commit ~branch_name:commit_branch >>| fun () -> cached

(** Check if a remote with a given tag exists in the cache as a branch, and clone to cache if it
    doesn't exist *)
let check_package_cache_branch ~repo ~ref ~commit () =
  let commit_branch = commit_branch_name ~commit in
  let branch = cache_branch_name ~ref in
  git_commit_branch_exists_or_create ~repo ~ref ~branch ~commit ~commit_branch () >>| fun cached ->
  (commit_branch, cached)

(** Clone to output_dir using the cache *)
let clone_from_cache ~output_dir ~repo (cache_branch_name, cached) =
  OS.Dir.delete ~recurse:true output_dir >>= fun () ->
  Exec.git_clone ~branch:cache_branch_name ~remote:(Fpath.to_string repo) ~output_dir >>| fun () ->
  cached

let clone_to ~output_dir ~remote ~ref ~commit () =
  get_cache_directory ~remote () >>= fun repo ->
  check_package_cache_branch ~repo ~ref ~commit () >>= clone_from_cache ~output_dir ~repo
