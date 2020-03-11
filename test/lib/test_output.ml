open Mdx.Output

let test_of_string =
  let make_test ~line ~expected =
    let test_name = Printf.sprintf "of_string: %S" line in
    let test_fun () =
      let actual = Line.of_string line in
      Alcotest.(check Testable.output_line) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~line:"" ~expected:[ S "" ];
    make_test ~line:"foo" ~expected:[ S "foo" ];
    make_test ~line:".." ~expected:[ S ".." ];
    make_test ~line:"..." ~expected:[ Ellipsis ];
    make_test ~line:"...." ~expected:[ Ellipsis; S "." ];
    make_test ~line:"foo..." ~expected:[ S "foo"; Ellipsis ];
    make_test ~line:"...foo" ~expected:[ Ellipsis; S "foo" ];
    make_test ~line:"foo ... bar ... pwac"
      ~expected:[ S "foo "; Ellipsis; S " bar "; Ellipsis; S " pwac" ];
    make_test ~line:"...a line with ... sub-ellipsis ..."
      ~expected:
        [ Ellipsis; S "a line with "; Ellipsis; S " sub-ellipsis "; Ellipsis ];
  ]

let test_equal =
  let make_test ~name ~input ~ref ~expected =
    let test_name = Printf.sprintf "equal: %s" name in
    let test_fun () =
      let actual = Lines.equal input ~ref in
      Alcotest.(check bool) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"empty" ~input:[] ~ref:[] ~expected:true;
    make_test ~name:"empty = ellipsis" ~input:[] ~ref:[ [ Ellipsis ] ]
      ~expected:true;
    make_test ~name:"foo = ellipsis" ~input:[ "foo" ] ~ref:[ [ Ellipsis ] ]
      ~expected:true;
    make_test ~name:"bar /= rab" ~input:[ "foo/bar/foo" ]
      ~ref:[ [ S "foo/rab/foo" ] ] ~expected:false;
    make_test ~name:"bar = sub_ellipsis" ~input:[ "foo/bar/foo" ]
      ~ref:[ [ S "foo/"; Ellipsis; S "/foo" ] ]
      ~expected:true;
    make_test ~name:"number list"
      ~input:[ "1"; "2"; "3"; "4"; "5"; "6" ]
      ~ref:[ [ S "1" ]; [ Ellipsis ]; [ S "4" ]; [ Ellipsis ]; [ S "6" ] ]
      ~expected:true;
    make_test ~name:"many ellipsis" ~expected:true
      ~input:[ "fooo"; "this is a line with a few sub-ellipsis !" ]
      ~ref:
        [
          [ Ellipsis ];
          [ Ellipsis; S "a line with "; Ellipsis; S " sub-ellipsis "; Ellipsis ];
        ];
  ]

let test_merge =
  let make_test ~name ~input ~ref ~expected =
    let test_name = Printf.sprintf "merge: %s" name in
    let test_fun () =
      let actual = Lines.merge input ref in
      Alcotest.(check (list Testable.output_line)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"number list"
      ~input:[ "1"; "2"; "3"; "4"; "5"; "6" ]
      ~ref:[ [ S "1" ]; [ Ellipsis ]; [ S "4" ]; [ Ellipsis ]; [ S "6" ] ]
      ~expected:[ [ S "1" ]; [ Ellipsis ]; [ S "4" ]; [ Ellipsis ]; [ S "6" ] ];
    make_test ~name:"many ellipsis (1)"
      ~input:[ "this is a line with a few sub-ellipsis !" ]
      ~ref:
        [
          [ Ellipsis; S "a line with "; Ellipsis; S " sub-ellipsis "; Ellipsis ];
        ]
      ~expected:
        [
          [ Ellipsis; S "a line with "; Ellipsis; S " sub-ellipsis "; Ellipsis ];
        ];
    make_test ~name:"many ellipsis (2)"
      ~input:[ "fooo"; "this is a line with a few sub-ellipsis !" ]
      ~ref:
        [
          [ Ellipsis ];
          [ Ellipsis; S "a line with "; Ellipsis; S " sub-ellipsis "; Ellipsis ];
        ]
      ~expected:
        [
          [ Ellipsis ];
          [ Ellipsis; S "a line with "; Ellipsis; S " sub-ellipsis "; Ellipsis ];
        ];
    make_test ~name:"diff" ~input:[ "def" ] ~ref:[ [ S "abc"; Ellipsis ] ]
      ~expected:[ [ S "def" ] ];
  ]

let suite = ("Output", test_of_string @ test_equal @ test_merge)
