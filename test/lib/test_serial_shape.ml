open Duniverse_lib
open Import

let shape_testable shape =
  Alcotest.testable (Serial_shape.pp shape) (Serial_shape.equal shape)

let int_as_string_conv =
  Serial_shape.Conv.make
    ~from_repr:(fun s ->
      match int_of_string_opt s with
      | Some i -> Ok i
      | None -> Rresult.R.error_msgf "Expected an int but got %s" s)
    ~to_repr:Int.to_string ~equal:Int.equal ~pp:Fmt.int ()

let test_from_opam_val =
  let make_test :
      type a.
      name:string ->
      shape:a Serial_shape.t ->
      expected:(a, Rresult.R.msg) result ->
      value:string ->
      unit Alcotest.test_case =
   fun ~name ~shape ~expected ~value ->
    let test_name = Printf.sprintf "Serial_shape.from_opam_val: %s" name in
    let test_fun () =
      let value = OpamParser.FullPos.value_from_string value "test.opam" in
      let actual = Serial_shape.from_opam_val shape value in
      Alcotest.(check (result (shape_testable shape) Testable.r_msg))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"bool" ~shape:Serial_shape.bool ~expected:(Ok true)
      ~value:{|true|};
    make_test ~name:"string" ~shape:Serial_shape.string ~expected:(Ok "abc")
      ~value:{|"abc"|};
    make_test ~name:"pair"
      ~shape:Serial_shape.(pair bool string)
      ~expected:(Ok (true, "abc"))
      ~value:{|[true "abc"]|};
    make_test ~name:"list"
      ~shape:Serial_shape.(list bool)
      ~expected:(Ok [ true; false; true ])
      ~value:{|[true false true]|};
    make_test ~name:"conv"
      ~shape:Serial_shape.(conv int_as_string_conv string)
      ~expected:(Ok 123) ~value:{|"123"|};
    make_test ~name:"choice c1"
      ~shape:Serial_shape.(choice3 bool string (list string))
      ~expected:(Ok (`C1 true))
      ~value:{|true|};
    make_test ~name:"choice c2"
      ~shape:Serial_shape.(choice3 bool string (list string))
      ~expected:(Ok (`C2 "abc"))
      ~value:{|"abc"|};
    make_test ~name:"choice c3"
      ~shape:Serial_shape.(choice3 bool string (list string))
      ~expected:(Ok (`C3 [ "abc"; "def" ]))
      ~value:{|["abc" "def"]|};
    make_test ~name:"ambiguous choice"
      ~shape:Serial_shape.(choice3 (pair string string) (list string) bool)
      ~expected:(Ok (`C1 ("abc", "def")))
      ~value:{|["abc" "def"]|};
    make_test ~name:"ambiguous choice with swapped priorities"
      ~shape:Serial_shape.(choice3 (list string) (pair string string) bool)
      ~expected:(Ok (`C1 [ "abc"; "def" ]))
      ~value:{|["abc" "def"]|};
  ]

let test_to_opam_val =
  let make_test :
      type a.
      name:string ->
      shape:a Serial_shape.t ->
      expected:string ->
      value:a ->
      unit Alcotest.test_case =
   fun ~name ~shape ~expected ~value ->
    let test_name = Printf.sprintf "Serial_shape.to_opam_val: %s" name in
    let test_fun () =
      let actual = Serial_shape.to_opam_val shape value in
      let actual = OpamPrinter.FullPos.value actual in
      Alcotest.(check string) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"bool" ~shape:Serial_shape.bool ~expected:{|true|}
      ~value:true;
    make_test ~name:"string" ~shape:Serial_shape.string ~expected:{|"abc"|}
      ~value:"abc";
    make_test ~name:"pair"
      ~shape:Serial_shape.(pair bool string)
      ~expected:{|[true "abc"]|} ~value:(true, "abc");
    make_test ~name:"list"
      ~shape:Serial_shape.(list bool)
      ~expected:{|[true false true]|} ~value:[ true; false; true ];
    make_test ~name:"conv"
      ~shape:Serial_shape.(conv int_as_string_conv string)
      ~expected:{|"123"|} ~value:123;
  ]

let test_cmdliner_parse =
  let make_test :
      type a.
      name:string ->
      shape:a Serial_shape.t ->
      expected:(a, Rresult.R.msg) result ->
      value:string ->
      unit Alcotest.test_case =
   fun ~name ~shape ~expected ~value ->
    let test_name =
      Printf.sprintf "Serial_shape.cmdliner_conv: parse %s" name
    in
    let test_fun () =
      let conv = Serial_shape.cmdliner_conv shape in
      let actual = Cmdliner.Arg.conv_parser conv value in
      Alcotest.(check (result (shape_testable shape) Testable.r_msg))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"bool" ~shape:Serial_shape.bool ~expected:(Ok true)
      ~value:"true";
    make_test ~name:"string" ~shape:Serial_shape.string ~expected:(Ok "abc")
      ~value:"abc";
    make_test ~name:"pair"
      ~shape:Serial_shape.(pair bool string)
      ~expected:(Ok (true, "abc"))
      ~value:"[true,abc]";
    make_test ~name:"list"
      ~shape:Serial_shape.(list bool)
      ~expected:(Ok [ true; false; true ])
      ~value:"[true,false,true]";
    make_test ~name:"list without delim"
      ~shape:Serial_shape.(list bool)
      ~expected:
        (Rresult.R.error_msg "list or pairs must be delimited by '[' and ']'")
      ~value:"true,false,true";
    make_test ~name:"choice c1"
      ~shape:Serial_shape.(choice3 bool (list bool) string)
      ~expected:(Ok (`C1 true))
      ~value:"true";
    make_test ~name:"choice c2"
      ~shape:Serial_shape.(choice3 bool (list bool) string)
      ~expected:(Ok (`C2 [ true; false ]))
      ~value:"[true,false]";
    make_test ~name:"choice c3"
      ~shape:Serial_shape.(choice3 bool (list bool) string)
      ~expected:(Ok (`C3 "abc"))
      ~value:"abc";
    make_test ~name:"ambiguous choice"
      ~shape:Serial_shape.(choice3 bool (pair bool bool) (list bool))
      ~expected:(Ok (`C2 (true, false)))
      ~value:"[true,false]";
    make_test ~name:"ambiguous choice 2"
      ~shape:Serial_shape.(choice3 bool (list bool) (pair bool bool))
      ~expected:(Ok (`C2 [ true; false ]))
      ~value:"[true,false]";
    make_test ~name:"list within list"
      ~shape:Serial_shape.(list (list bool))
      ~expected:(Ok [ [ true ]; [ true; false ]; [ false ] ])
      ~value:"[[true],[true,false],[false]]";
    make_test ~name:"empty list"
      ~shape:Serial_shape.(list bool)
      ~expected:(Ok []) ~value:"";
    make_test ~name:"unterminated list"
      ~shape:Serial_shape.(list string)
      ~expected:(Error (`Msg "unmatched list delimiter '['"))
      ~value:"[abc,def";
    make_test ~name:"unterminated inner list"
      ~shape:Serial_shape.(list (list string))
      ~expected:(Error (`Msg "unmatched list delimiter '['"))
      ~value:"[[abc,def],[uvw,xyz,[rst]]";
    make_test ~name:"floating ]"
      ~shape:Serial_shape.(list string)
      ~expected:(Error (`Msg "unmatched list delimiter ']'"))
      ~value:"[abc,def]]";
    make_test ~name:"conv"
      ~shape:Serial_shape.(conv int_as_string_conv string)
      ~expected:(Ok 123) ~value:"123";
  ]

let test_cmdliner_print =
  let make_test :
      type a.
      name:string ->
      shape:a Serial_shape.t ->
      value:a ->
      expected:string ->
      unit Alcotest.test_case =
   fun ~name ~shape ~value ~expected ->
    let test_name =
      Printf.sprintf "Serial_shape.cmdliner_conv: print %s" name
    in
    let test_fun () =
      let conv = Serial_shape.cmdliner_conv shape in
      let actual = Fmt.str "%a" (Cmdliner.Arg.conv_printer conv) value in
      Alcotest.(check string) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"bool" ~shape:Serial_shape.bool ~expected:"true" ~value:true;
    make_test ~name:"string" ~shape:Serial_shape.string ~expected:"abc"
      ~value:"abc";
    make_test ~name:"pair"
      ~shape:Serial_shape.(pair bool string)
      ~expected:"[true,abc]" ~value:(true, "abc");
    make_test ~name:"list"
      ~shape:Serial_shape.(list bool)
      ~expected:"[true,false,true]" ~value:[ true; false; true ];
    make_test ~name:"conv"
      ~shape:Serial_shape.(conv int_as_string_conv string)
      ~expected:"123" ~value:123;
  ]

let suite =
  ( "Serial_shape",
    List.concat
      [
        test_from_opam_val;
        test_to_opam_val;
        test_cmdliner_parse;
        test_cmdliner_print;
      ] )
