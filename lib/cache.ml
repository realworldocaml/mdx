open Bos
open Rresult
open Astring

let valid_remote_name = String.map (function '/' | '@' | ':' -> '-' | x -> x)

let git_remote_exists_or_create ~remotes ~remote =
  let remote_name = valid_remote_name remote in
  match String.Set.mem remote_name remotes with
  | true -> Ok ()
  | false -> Exec.git_remote_add ~remote_url:remote ~remote_name

let git_branch_exists_or_create ~remotes ~branch ~remote ~tag cache_dir =
  let remote_name = valid_remote_name remote in
  OS.Dir.with_current cache_dir
    (fun () ->
      OS.Path.exists Fpath.(v ".git" / "refs" / "heads" / branch) >>= function
      | true -> Ok ()
      | false -> (
          git_remote_exists_or_create ~remotes ~remote >>= function
          | () -> Exec.git_fetch_to ~remote_name ~tag ~branch ) )
    ()
  |> R.join

let cache_dir =
  let ( / ) = Filename.concat in
  let default_cache_dir () =
    let os_cache_dir = if Sys.win32 then "Local Settings" / "Cache" else ".cache" in
    Bos.OS.Env.req_var "HOME" >>| fun home -> home / os_cache_dir
  in
  let xdg_cache_dir () =
    match Bos.OS.Env.var "XDG_CACHE_HOME" with Some x -> Ok x | None -> default_cache_dir ()
  in
  (match Bos.OS.Env.var "DUNIVERSE_CACHE" with Some x -> Ok x | None -> xdg_cache_dir ())
  >>= fun cache_dir -> Fpath.of_string (cache_dir / "duniverse")

let git_get ~output_dir ~remote ~tag () =
  cache_dir >>= fun cache_location ->
  let cache_branch_name = valid_remote_name remote ^ "-" ^ tag in
  (OS.Dir.exists cache_location >>= function
   | true -> Ok ()
   | false -> Exec.git_init cache_location)
  >>= fun () ->
  Exec.git_remotes cache_location >>= fun remotes ->
  git_branch_exists_or_create ~remotes ~branch:cache_branch_name ~remote ~tag
    cache_location
  >>= fun () ->
  OS.Dir.delete ~recurse:true output_dir >>= fun () ->
  Exec.git_clone ~branch:cache_branch_name ~remote:(Fpath.to_string cache_location) ~output_dir
  >>= fun () ->
  OS.Dir.with_current output_dir
    (fun () ->
      Exec.git_rename_current_branch_to ~branch:tag >>= fun () ->
      Exec.git_remote_add ~remote_name:"upstream" ~remote_url:remote >>= fun () ->
      Exec.git_remote_remove ~remote_name:"origin" )
    ()
  |> R.join
