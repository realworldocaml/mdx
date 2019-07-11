let test_parse_ls_remote_line =
  let make_test ~name ~line ~expected () =
    let test_name = Printf.sprintf "parse_ls_remote_line: %s" name in
    let test_fun () =
      let actual = Duniverse_lib.Git.parse_ls_remote_line line in
      Alcotest.(check (result (pair string string) Testable.r_msg)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~name:"Ok" ~line:"12ab  refs/tags/v1" ~expected:(Ok ("12ab", "refs/tags/v1")) ();
    make_test ~name:"Error" ~line:"12ab  refs/tags/v1 something"
      ~expected:
        (Error (`Msg "Invalid git ls-remote output line: \"12ab  refs/tags/v1 something\""))
      ()
  ]

let suite = ("Git", test_parse_ls_remote_line)
