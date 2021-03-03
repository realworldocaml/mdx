open Import

type t = Fpath.t

let folder_blacklist = [ "_build"; "_opam"; Fpath.to_string Config.vendor_dir ]

(* We accept a filter here instead of filtering the result because
   some repos have duplicate dummy opam files that would fail the uniqueness
   check. Dune is a good example, the blackbox tests contain duplicate opam files *)
let local_packages ~recurse ?filter t =
  let open Result.O in
  Bos.OS.Dir.exists t >>= fun exists ->
  if not exists then Ok String.Map.empty
  else
    let traverse =
      if recurse then
        `Sat
          (fun p ->
            Ok
              (not
                 (List.mem
                    (Fpath.to_string (Fpath.base p))
                    ~set:folder_blacklist)))
      else `Sat (fun p -> Ok (Fpath.equal p t))
    in
    let accept_name =
      match filter with
      | None -> fun _ -> true
      | Some list ->
          fun p ->
            let pkg =
              p |> Fpath.rem_ext |> Fpath.basename |> OpamPackage.Name.of_string
            in
            List.exists ~f:(fun name -> OpamPackage.Name.equal name pkg) list
    in
    Bos.OS.Path.fold
      ~elements:(`Sat (fun p -> Ok (Fpath.has_ext ".opam" p && accept_name p)))
      ~traverse
      (fun path acc -> Fpath.(basename (rem_ext path), t // path) :: acc)
      [] [ t ]
    >>= fun lst ->
    String.Map.of_list lst
    |> Result.map_error ~f:(fun (_, a, b) ->
           `Msg
             (Printf.sprintf
                "Conflicting definitions for the same package have been found:\n\
                 - %s\n\
                 - %s" (Fpath.to_string a) (Fpath.to_string b)))

let dune_project t = Fpath.(t / "dune-project")

let project_name t =
  let open Result.O in
  let dune_project = dune_project t in
  Dune_file.Raw.as_sexps dune_project >>= Dune_file.Project.name

let lockfile ~name t = Fpath.(t / (name ^ Config.lockfile_ext))

let lockfile ?local_packages:lp t =
  let open Result.O in
  let local_packages =
    match lp with
    | Some lp -> Ok lp
    | None ->
        local_packages ~recurse:false t >>| fun lp ->
        lp |> String.Map.keys |> List.map ~f:OpamPackage.Name.of_string
  in

  local_packages >>= fun names ->
  match names with
  | [ name ] ->
      let name = OpamPackage.Name.to_string name in
      Ok (lockfile ~name t)
  | _ -> project_name t >>= fun name -> Ok (lockfile ~name t)
