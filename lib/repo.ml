open Rresult
open Astring

type t = Fpath.t

let local_packages ~recurse t =
  Bos.OS.Dir.exists t >>= fun exists ->
  if not exists then Ok String.Map.empty
  else
    let traverse =
      if recurse then
        `Sat (fun p -> Ok (Fpath.to_string p <> "duniverse"))
      else
        `Sat (fun p -> Ok (Fpath.equal p t))
    in
    Bos.OS.Path.fold
      ~elements:(`Sat (fun p -> Ok (Fpath.has_ext ".opam" p)))
      ~traverse
      (fun path acc -> Fpath.(basename (rem_ext path), t // path) :: acc)
      [] [ t ]
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
    | None ->
      local_packages ~recurse:false t >>| fun lp ->
      List.map
        (fun (name, _) -> {Types.Opam.name; version = None})
        (String.Map.bindings lp)
  in
  local_packages >>= fun pkgs ->
  match pkgs with
  | [{name; _}] -> Ok (duniverse_file ~name t)
  | _ ->
    project_name t >>= fun name ->
    Ok (duniverse_file ~name t)
