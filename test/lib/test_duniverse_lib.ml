let () =
  Alcotest.run "Duniverse"
    [
      Test_dev_repo.suite;
      Test_dune_file.suite;
      Test_duniverse.suite;
      Test_git.suite;
      Test_opam.suite;
      Test_parallel.suite;
      Test_pin_depends.suite;
      Test_serial_shape.suite;
      Test_source_opam_config.suite;
      Test_uri_utils.suite;
    ]
