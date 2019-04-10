let () =
  Alcotest.run
    "Duniverse"
    [ "Opam_cmd", Test_opam_cmd.test_tag_from_archive
    ]
