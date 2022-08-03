open Mdx.Part

let parse_parts =
  let equal = ( = ) in
  let pp fmt = function
    | Parse_parts.Normal s -> Fmt.pf fmt "Normal: %S" s
    | Compat_attr (n, sep_indent) ->
        Fmt.pf fmt "Compat_attr (%S, %S)" n sep_indent
    | Part_begin (n, sep_indent) ->
        Fmt.pf fmt "Part_begin (%S, %S)" n sep_indent
    | Part_end prefix -> Fmt.pf fmt "Part_end %a" Fmt.(option string) prefix
  in
  Alcotest.testable pp equal

let test_of_line =
  let make_test ~line ~expected () =
    let test_name = Printf.sprintf "parse_line: %S" line in
    let test_fun () =
      let actual = Parse_parts.parse_line line in
      Alcotest.(check parse_parts) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~line:"foo" ~expected:(Normal "foo") ();
    make_test ~line:"    (* $MDX part-begin=bar *)    "
      ~expected:(Part_begin ("bar", "    "))
      ();
    make_test ~line:"(* $MDX part-begin=bar     "
      ~expected:(Normal "(* $MDX part-begin=bar     ") ();
    make_test ~line:"   (* $MDX part-end   *)   " ~expected:(Part_end None) ();
    make_test ~line:"    [@@@part \"foobar\"]    "
      ~expected:(Compat_attr ("foobar", "    "))
      ();
  ]

let suite = ("Parts", test_of_line)
