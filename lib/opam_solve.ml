module Solver = Opam_0install.Solver.Make(Opam_0install.Dir_context)

let calculate ~opam_repo ~opam_files =
  let module OR = Osrelease in
  let env =
    Opam_0install.Dir_context.std_env
      ~arch:(OR.Arch.to_string `X86_64)
      ~os:(OR.OS.to_string `Linux)
      ~os_distribution:(OR.Distro.to_string (`Linux `Debian))
      ~os_version:"10.0"
      ~os_family:(OR.Distro.to_string (`Linux `Debian))
      ~sys_ocaml_version:Ocaml_version.(Releases.latest |> to_string) (* TODO generalise *)
      ()
  in
 let local_packages =
    List.fold_left
      (fun acc path -> 
        let name = Filename.basename path |> Filename.chop_extension |> OpamPackage.Name.of_string in
        let version =
        let dir = Filename.dirname path in
        let dev = OpamPackage.Version.of_string "dev" in
        if dir = "." then dev else
          match OpamPackage.of_string_opt dir with
            | Some { OpamPackage.version; _ } -> version
            | None -> dev
        in
        let file = OpamFile.OPAM.read (OpamFilename.of_string path |> OpamFile.make) in
        OpamPackage.Name.Map.add name (version, file) acc) OpamPackage.Name.Map.empty opam_files
  in
  let local_package_names = OpamPackage.Name.(Map.keys local_packages |> Set.of_list) in
  let context =
    Opam_0install.Dir_context.create (Fpath.to_string Fpath.(opam_repo / "packages"))
      ~test:local_package_names
      ~pins:local_packages
      ~constraints:OpamPackage.Name.Map.empty
      ~env in
  let result = Solver.solve context (OpamPackage.Name.Map.keys local_packages) in
  match result with
  | Error e -> Error (`Msg (Solver.diagnostics e))
  | Ok selections -> Ok (Solver.packages_of_result selections)

