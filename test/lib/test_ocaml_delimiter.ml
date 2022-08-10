open Mdx.Ocaml_delimiter

let test_parse =
  let make_test ~line ~expected =
    let test_name = Printf.sprintf "parse: %S" line in
    let test_fun () =
      let actual = parse line in
      Alcotest.(check (result (list Testable.ocaml_delimiter) Testable.msg))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~line:"" ~expected:(Ok [ Content "" ]);
    make_test ~line:"foo" ~expected:(Ok [ Content "foo" ]);
    make_test ~line:"    (* $MDX part-begin=bar *)    "
      ~expected:(Ok [ Part_begin { sep_indent = "    "; name = "bar" } ]);
    (* Unclosed comments are caught by syntax highlighting, it is okay to
       silently ignore them and not print a warning here. *)
    make_test ~line:"(* $MDX part-begin=bar     "
      ~expected:(Ok [ Content "(* $MDX part-begin=bar     " ]);
    make_test ~line:"   (* $MDX part-end   *)   " ~expected:(Ok [ Part_end ]);
    make_test ~line:"    [@@@part \"foobar\"]    "
      ~expected:(Ok [ Compat_attr { sep_indent = "    "; name = "foobar" } ]);
    make_test ~line:"(* $MDX foo *)"
      ~expected:
        (Error (`Msg "'(* $MDX foo *)' is not a valid ocaml delimiter for mdx."));
    make_test ~line:"[@@@foo \"bar\"]"
      ~expected:(Ok [ Content "[@@@foo \"bar\"]" ]);
    make_test ~line:"(* $MDX part-end=foo *)"
      ~expected:
        (Error
           (`Msg
             "'part-end' delimiter does not accept a value. Please write '(* \
              $MDX part-end *)' instead."));
  ]

let suite = ("Ocaml_delimiter", test_parse)
