module Testable = struct
  open Mdx.Label

  let relation = Alcotest.testable Relation.pp ( = )
end

let test_raw_parse =
  let ty = Alcotest.(pair string (option (pair Testable.relation string))) in
  let make_test ~input ~expected =
    let test_name = Printf.sprintf "raw_parse: %S" input in
    let test_fun () =
      Alcotest.check ty test_name expected (Mdx.Label.Relation.raw_parse input)
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~input:"" ~expected:("", None)
  ; make_test ~input:"foo=" ~expected:("foo", Some (Eq, ""))
  ; make_test ~input:"foo<>bar" ~expected:("foo", Some (Neq, "bar"))
  ; make_test ~input:"<bar" ~expected:("", Some (Lt, "bar"))
  ; make_test ~input:"foo<=bar" ~expected:("foo", Some (Le, "bar"))
  ; make_test ~input:"foo>bar" ~expected:("foo", Some (Gt, "bar"))
  ; make_test ~input:">=" ~expected:("", Some (Ge, ""))
  ]

let suite = ("Label", test_raw_parse)
