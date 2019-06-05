let () =
  Alcotest.run "Duniverse"
    [ Test_opam_cmd.suite;
      Test_git.suite;
      Test_opam.suite;
      ("Opam_show_result", Test_opam_show_result.test_make)
    ]
