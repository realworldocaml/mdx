open Rresult
open Astring

type t = Fpath.t

let local_packages t =
  Bos.OS.Dir.exists t >>= fun exists -> if not exists then Ok String.Map.empty else
  Bos.OS.Dir.contents ~rel:true t
  >>| List.filter (Fpath.has_ext ".opam")
  >>| List.map (fun path -> Fpath.(to_string (rem_ext path), t // path))
  >>| String.Map.of_list

let dune_project t = Fpath.(t / "dune-project")

let project_name t =
  let dune_project = dune_project t in
  Dune_file.Raw.as_sexps dune_project >>= Dune_file.Project.name

let duniverse_file ~name t =
  Fpath.(t / (name ^ ".opam.locked"))

let duniverse_file ?local_packages:lp t =
  let local_packages =
    match lp with
    | Some lp -> Ok lp
    | None -> local_packages t
  in
  local_packages >>= fun pkgs ->
  match String.Map.bindings pkgs with
  | [(name, _)] -> Ok (duniverse_file ~name t)
  | _ ->
    project_name t >>= fun name ->
    Ok (duniverse_file ~name t)
