let () =
  Alcotest.run "Mdx"
    [ Test_block.suite
    ; Test_library.suite
    ; Test_syntax.suite
    ]
