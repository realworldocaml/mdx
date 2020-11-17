module Lang = struct
  let parse_version =
    let make_test ~version ~expected () =
      let test_name = Printf.sprintf "Lang.parse_version: %s" version in
      let test_fun () =
        let actual = Duniverse_lib.Dune_file.Lang.parse_version version in
        Alcotest.(check (result (pair int int) Testable.r_msg)) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~version:"1.6" ~expected:(Ok (1, 6)) ();
      make_test ~version:"1.x" ~expected:(Error (`Msg "Invalid dune lang version: 1.x")) ();
      make_test ~version:"ver" ~expected:(Error (`Msg "Invalid dune lang version: ver")) ();
    ]

  let parse_stanza =
    let make_test ~stanza ~expected () =
      let test_name = Printf.sprintf "Lang.parse_stanza: %s" stanza in
      let test_fun () =
        let actual = Duniverse_lib.Dune_file.Lang.parse_stanza stanza in
        Alcotest.(check (result (pair int int) Testable.r_msg)) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~stanza:"(lang dune 1.6)" ~expected:(Ok (1, 6)) ();
      make_test ~stanza:"(lang dune ver)"
        ~expected:(Error (`Msg "Invalid dune lang version: ver"))
        ();
      make_test ~stanza:"(lang something)"
        ~expected:(Error (`Msg "Invalid lang stanza: (lang something)"))
        ();
    ]

  let is_stanza =
    let make_test ~line ~expected () =
      let test_name = Printf.sprintf "Lang.is_stanza: %s" line in
      let test_fun () =
        let actual = Duniverse_lib.Dune_file.Lang.is_stanza line in
        Alcotest.(check bool) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~line:"; a comment" ~expected:false ();
      make_test ~line:"(name project)" ~expected:false ();
      make_test ~line:"(lang dune 1.6)" ~expected:true ();
    ]
end

let suite = ("Dune_file", Lang.parse_version @ Lang.parse_stanza @ Lang.is_stanza)
