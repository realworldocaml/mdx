let test_repo_name =
  let make_test ~dev_repo ~expected () =
    let test_name = Printf.sprintf "repo_name: %s" dev_repo in
    let test_fun () =
      let actual = Duniverse_lib.Dev_repo.(repo_name (from_string dev_repo)) in
      Alcotest.(check string) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~dev_repo:"git://github.com/ocamllabs/opam-monorepo" ~expected:"opam-monorepo" ();
    make_test ~dev_repo:"git://github.com/ocamllabs/opam-monorepo.git" ~expected:"opam-monorepo" ();
    make_test ~dev_repo:"git+https://github.com/ocamllabs/opam-monorepo.git"
      ~expected:"opam-monorepo" ();
  ]

let suite = ("Dev_repo", List.concat [ test_repo_name ])
