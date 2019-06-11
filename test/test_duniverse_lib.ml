let () =
  Alcotest.run "Duniverse"
    [ Test_opam_cmd.suite; Test_uri_utils.suite; Test_opam.suite; Test_opam_show_result.suite ]
