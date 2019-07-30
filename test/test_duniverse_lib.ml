let () =
  Alcotest.run "Duniverse"
    [ Test_opam_cmd.suite;
      Test_uri_utils.suite;
      Test_opam.suite;
      Test_duniverse.suite;
      Test_opam_show_result.suite;
      Test_dune_file.suite;
      Test_git.suite;
      Test_parallel.suite
    ]
