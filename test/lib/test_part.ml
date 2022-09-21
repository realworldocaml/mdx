open Mdx.Part.Internal

let parse_parts =
  let equal = ( = ) in
  let pp fmt = function
    | Parse_parts.Content s -> Fmt.pf fmt "Content: %S" s
    | Compat_attr { name; sep_indent } ->
        Fmt.pf fmt "Compat_attr {name=%S; sep_indent=%S}" name sep_indent
    | Part_begin { name; sep_indent } ->
        Fmt.pf fmt "Part_begin {name=%S; sep_indent=%S}" name sep_indent
    | Part_end -> Fmt.pf fmt "Part_end"
  in
  Alcotest.testable pp equal

let test_of_line =
  let make_test ~line ~expected =
    let test_name = Printf.sprintf "parse: %S" line in
    let test_fun () =
      let actual = Parse_parts.parse line in
      Alcotest.(check (Testable.errormsg (list parse_parts)))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~line:"" ~expected:(Ok [ Content "" ]);
    make_test ~line:"foo" ~expected:(Ok [ Content "foo" ]);
    make_test ~line:"    (* $MDX part-begin=bar *)    "
      ~expected:(Ok [ Part_begin { name = "bar"; sep_indent = "    " } ]);
    make_test ~line:"(* $MDX part-begin=bar     "
      ~expected:(Ok [ Content "(* $MDX part-begin=bar     " ]);
    make_test ~line:"   (* $MDX part-end   *)   " ~expected:(Ok [ Part_end ]);
    make_test ~line:"    [@@@part \"foobar\"]    "
      ~expected:(Ok [ Compat_attr { name = "foobar"; sep_indent = "    " } ]);
    make_test ~line:"[@@@foo \"bar\"]"
      ~expected:(Ok [ Content "[@@@foo \"bar\"]" ]);
    make_test ~line:"(* $MDX foo *)"
      ~expected:
        (Error (`Msg "'(* $MDX foo *)' is not a valid ocaml delimiter for mdx."));
    make_test ~line:"(* $MDX part-end=foo *)"
      ~expected:
        (Error
           (`Msg
             "'part-end' delimiter does not accept a value. Please write '(* \
              $MDX part-end *)' instead."));
  ]

let suite = ("Parts", test_of_line)
