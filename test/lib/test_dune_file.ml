module Lang = struct
  let from_content =
    let make_test ~name ~content ~expected () =
      let test_name = Printf.sprintf "Lang.from_content: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Dune_file.Lang.from_content content in
        Alcotest.(check (result (pair int int) Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    let invalid_dune_project =
      Error
        (`Msg
          "Invalid dune-project file: It does not start with a valid lang \
           stanza")
    in
    [
      make_test ~name:"Empty dune-project" ~content:""
        ~expected:invalid_dune_project ();
      make_test ~name:"Valid version"
        ~expected:(Ok (1, 2))
        ~content:"(lang dune 1.2)" ();
      make_test ~name:"Newlines"
        ~expected:(Ok (1, 2))
        ~content:"(lang dune 1.2)\n(name my-project)\n\n" ();
      make_test ~name:"Windows newlines"
        ~expected:(Ok (1, 2))
        ~content:"(lang dune 1.2)\r\n(name my-project)\r\n\r\n" ();
      make_test ~name:"Invalid version"
        ~expected:
          (Error
             (`Msg
               "Invalid dune-project file: invalid lang version \
                1.999999999999999999999999999"))
        ~content:"(lang dune 1.999999999999999999999999999)" ();
      make_test ~name:"Invalid lang stanza" ~expected:invalid_dune_project
        ~content:"(lang dune ver)\n(name my-project)\n" ();
      make_test ~name:"Does not start with lang stanza"
        ~expected:invalid_dune_project
        ~content:"\n\n(lang dune ver)\n(name my-project)\n" ();
    ]

  let update =
    let make_test ~name ~version ~content ~expected () =
      let test_name = Printf.sprintf "Lang.update: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Dune_file.Lang.update ~version content in
        Alcotest.(check string) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"No stanza" ~version:(1, 3) ~expected:"(name my-project)"
        ~content:"(name my-project)" ();
      make_test ~name:"Replace" ~version:(1, 3) ~expected:"(lang dune 1.3)"
        ~content:"(lang dune 1.2)" ();
      make_test ~name:"Newlines" ~version:(1, 3)
        ~expected:"(lang dune 1.3)\n(name my-project)\n"
        ~content:"(lang dune 1.2)\n(name my-project)\n" ();
      make_test ~name:"Windows newlines" ~version:(1, 3)
        ~expected:"(lang dune 1.3)\r\n(name my-project)\r\n"
        ~content:"(lang dune 1.2)\r\n(name my-project)\r\n" ();
    ]
end

let suite = ("Dune_file", List.concat [ Lang.from_content; Lang.update ])
