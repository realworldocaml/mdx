open Mdx.Part
module D = Mdx.Ocaml_delimiter

let parse_parts =
  let equal = ( = ) in
  let pp fmt = function
    | D.Content s -> Fmt.pf fmt "Content: %S" s
    | Compat_attr { name; sep_indent } ->
        Fmt.pf fmt "Compat_attr {name=%S; sep_indent=%S}" name sep_indent
    | Part_begin { name; sep_indent } ->
        Fmt.pf fmt "Part_begin {name=%S; sep_indent=%S}" name sep_indent
    | Part_end -> Fmt.pf fmt "Part_end"
  in
  Alcotest.testable pp equal

let test_of_line =
  let make_test ~line ~expected () =
    let test_name = Printf.sprintf "parse_line: %S" line in
    let test_fun () =
      let actual = Parse_parts.parse_line line in
      Alcotest.(check (list parse_parts)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~line:"foo" ~expected:[ Content "foo" ] ();
    make_test ~line:"    (* $MDX part-begin=bar *)    "
      ~expected:[ Part_begin { name = "bar"; sep_indent = "    " } ]
      ();
    make_test ~line:"(* $MDX part-begin=bar     "
      ~expected:[ Content "(* $MDX part-begin=bar     " ]
      ();
    make_test ~line:"   (* $MDX part-end   *)   " ~expected:[ Part_end ] ();
    make_test ~line:"    [@@@part \"foobar\"]    "
      ~expected:[ Compat_attr { name = "foobar"; sep_indent = "    " } ]
      ();
  ]

let suite = ("Parts", test_of_line)
