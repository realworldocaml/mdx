open Import

type t = Fpath.t

let folder_blacklist = [ "_build"; "_opam"; Fpath.to_string Config.vendor_dir ]

let local_packages ~recurse t =
  let open Result.O in
  Bos.OS.Dir.exists t >>= fun exists ->
  if not exists then Ok []
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
    Bos.OS.Path.fold
      ~elements:(`Sat (fun p -> Ok (Fpath.has_ext ".opam" p)))
      ~traverse
      (fun path acc ->
        let pkg_name =
          Fpath.(basename (rem_ext path)) |> OpamPackage.Name.of_string
        in
        Fpath.(pkg_name, t // path) :: acc)
      [] [ t ]

let all_local_packages t = local_packages ~recurse:true t

let dune_project t = Fpath.(t / "dune-project")

let name t =
  let open Result.O in
  let dune_project = dune_project t in
  Dune_file.Raw.as_sexps dune_project >>= Dune_file.Project.name

let lockfile ~name t = Fpath.(t / (name ^ Config.lockfile_ext))

let lockfile ~target_packages t =
  let open Result.O in
  match target_packages with
  | [ name ] ->
      let name = OpamPackage.Name.to_string name in
      Ok (lockfile ~name t)
  | _ -> name t >>= fun name -> Ok (lockfile ~name t)

let local_lockfiles repo =
  let open Result.O in
  Bos.OS.Dir.contents ~dotfiles:false repo >>| fun content ->
  List.filter
    ~f:(fun path ->
      let is_file =
        match Bos.OS.File.exists path with Ok b -> b | _ -> false
      in
      is_file && Fpath.has_ext Config.lockfile_ext path)
    content
