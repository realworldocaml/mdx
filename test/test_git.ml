let test_uri_has_git_extension =
  let make_test ~uri_str ~expected () =
    let test_name = Printf.sprintf "uri_has_git_extension: %s" uri_str in
    let uri = Uri.of_string uri_str in
    let test_fun () =
      let actual = Duniverse_lib.Git.uri_has_git_extension uri in
      Alcotest.(check bool) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~uri_str:"https://host.com/path/to/repo.git" ~expected:true ();
    make_test ~uri_str:"https://host.com/path/to/repo" ~expected:false ()
  ]

let suite = ("Git", test_uri_has_git_extension)
