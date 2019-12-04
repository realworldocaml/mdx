open Duniverse_lib

let test_make =
  let make_test ~name ~input ~expected =
    let test_name = Printf.sprintf "make: %s" name in
    let test_fun () =
      let expected_t = Opam_show_result.from_list expected in
      let actual = Rresult.R.get_ok (Opam_show_result.make input) in
      Alcotest.(check (module Opam_show_result) test_name expected_t actual)
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"empty" ~input:[] ~expected:[];
    make_test ~name:"1 field"
      ~input:[ "name       test"; "url   bonjour.com" ]
      ~expected:[ ("test", "url", "bonjour.com") ];
    make_test ~name:"1 field (field key with colon)"
      ~input:[ "name       test"; "url:   bonjour.com" ]
      ~expected:[ ("test", "url:", "bonjour.com") ];
    make_test ~name:"2 fields"
      ~input:[ "name       test"; "url:   bonjour.com"; {|field2   ["salut"]|} ]
      ~expected:[ ("test", "field2", {|["salut"]|}); ("test", "url:", "bonjour.com") ];
    make_test ~name:"with name field"
      ~input:[ "name       test"; "url:   bonjour.com"; "name   test"; {|field2   ["salut"]|} ]
      ~expected:[ ("test", "field2", {|["salut"]|}); ("test", "url:", "bonjour.com") ];
    make_test ~name:"multiple packages"
      ~input:[ "name       test2"; "url:   bonjour.com"; "name   test"; {|field2   ["salut"]|} ]
      ~expected:[ ("test", "field2", {|["salut"]|}); ("test2", "url:", "bonjour.com") ];
  ]

let suite = ("Opam_show_result", test_make)
