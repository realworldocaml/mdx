module Lang = struct
  let from_content =
    let make_test ~name ~content ~expected () =
      let test_name = Printf.sprintf "Lang.from_content: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Dune_file.Lang.from_content content in
        Alcotest.(check (result (option (pair int int)) Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Empty dune-project" ~content:"" ~expected:(Ok None) ();
      make_test ~name:"Valid version"
        ~expected:(Ok (Some (1, 2)))
        ~content:"(lang dune 1.2)" ();
      make_test ~name:"Newlines"
        ~expected:(Ok (Some (1, 2)))
        ~content:"(lang dune 1.2)\n(name my-project)\n\n" ();
      make_test ~name:"Windows newlines"
        ~expected:(Ok (Some (1, 2)))
        ~content:"(lang dune 1.2)\r\n(name my-project)\r\n\r\n" ();
      make_test ~name:"Invalid version"
        ~expected:
          (Error
             (`Msg "Invalid dune lang version: 1.999999999999999999999999999"))
        ~content:"(lang dune 1.999999999999999999999999999)" ();
      make_test ~name:"Multiple lang stanzas"
        ~expected:
          (Error (`Msg "Invalid dune-project file: Multiple lang stanzas"))
        ~content:"(lang dune 1.2)\n(name my-project)\n(lang dune 1.3)\n" ();
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

  let prepend =
    let make_test ~name ~version ~content ~expected () =
      let test_name = Printf.sprintf "Lang.prepend: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Dune_file.Lang.prepend ~version content in
        Alcotest.(check string) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Empty" ~version:(1, 3) ~expected:"(lang dune 1.3)\n"
        ~content:"" ();
      make_test ~name:"Windows newlines" ~version:(1, 3)
        ~expected:"(lang dune 1.3)\r\n(name my-project)\r\n"
        ~content:"(name my-project)\r\n" ();
    ]
end

let suite =
  ("Dune_file", List.concat [ Lang.from_content; Lang.update; Lang.prepend ])
