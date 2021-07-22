let version =
  let pp_version ppf v = Format.fprintf ppf "%s" (OpamPackage.Version.to_string v) in
  Alcotest.testable pp_version OpamPackage.Version.equal

let create_opam_file ~package ~version =
  Printf.ksprintf OpamPackage.of_string "%s.DUMMY" package
  |> OpamFile.OPAM.create
  |> OpamFile.OPAM.with_version_opt (Option.map OpamPackage.Version.of_string version)

let test_local_package_version =
  let test name ~explicit_version:explicit_version_string ~expected:expected_string ~opam_version =
    ( Printf.sprintf "local_package_version: %s" name,
      `Quick,
      fun () ->
        let opam_file = create_opam_file ~package:"PKG" ~version:opam_version in
        let explicit_version = Option.map OpamPackage.Version.of_string explicit_version_string in
        let expected = OpamPackage.Version.of_string expected_string in
        let got = Duniverse_lib.Types.Opam.local_package_version opam_file ~explicit_version in
        Alcotest.check version __LOC__ expected got )
  in
  [
    test "explicit version wins" ~explicit_version:(Some "EXPLICIT") ~opam_version:(Some "OPAMVER")
      ~expected:"EXPLICIT";
    test "if no explicit, use version in opam file" ~explicit_version:None
      ~opam_version:(Some "OPAMVER") ~expected:"OPAMVER";
    test "if all else fails, use zdev" ~explicit_version:None ~opam_version:None ~expected:"zdev";
  ]

let suite = ("Types.Opam", test_local_package_version)
