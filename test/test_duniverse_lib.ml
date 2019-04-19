let () =
  Alcotest.run
    "Duniverse"
    [ "Opam_cmd", Test_opam_cmd.test_tag_from_archive
    ; "Opam_show_result", Test_opam_show_result.test_make
    ]
