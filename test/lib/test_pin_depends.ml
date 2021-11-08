let t =
  Alcotest.testable Duniverse_lib.Pin_depends.pp Duniverse_lib.Pin_depends.equal

let test_sort_uniq =
  let convert (pkg, url) = (OpamPackage.of_string pkg, OpamUrl.parse url) in
  let make_test ~name ~input ~expected () =
    let test_name = Printf.sprintf "sort_uniq: %s" name in
    let test_fun () =
      let input = List.map convert input in
      let expected = Rresult.R.map (List.map convert) expected in
      let actual = Duniverse_lib.Pin_depends.sort_uniq input in
      Alcotest.(check (result (list t) Testable.r_msg))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"Empty" ~input:[] ~expected:(Ok []) ();
    make_test ~name:"Preserve"
      ~input:[ ("a.1", "git+https://repo.com") ]
      ~expected:(Ok [ ("a.1", "git+https://repo.com") ])
      ();
    make_test ~name:"Dedup"
      ~input:
        [ ("a.1", "git+https://repo.com"); ("a.1", "git+https://repo.com") ]
      ~expected:(Ok [ ("a.1", "git+https://repo.com") ])
      ();
    make_test ~name:"Different versions"
      ~input:
        [ ("a.1", "git+https://repo.com"); ("a.2", "git+https://repo.com") ]
      ~expected:
        (Rresult.R.error_msg
           "Package a is pinned to different versions/url:\n\
           \  - a.2: git+https://repo.com\n\
           \  - a.1: git+https://repo.com")
      ();
    make_test ~name:"Different URLs"
      ~input:
        [
          ("a.1", "git+https://repo.com#master");
          ("a.1", "git+https://repo.com#branch");
        ]
      ~expected:
        (Rresult.R.error_msg
           "Package a is pinned to different versions/url:\n\
           \  - a.1: git+https://repo.com#branch\n\
           \  - a.1: git+https://repo.com#master")
      ();
  ]

let suite = ("Pin_depends", List.concat [ test_sort_uniq ])
