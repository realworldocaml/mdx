let test_to_sexp_strict =
  let make_test ~name ~input ~expected () =
    let test_name = Printf.sprintf "to_sexp_strict: %s" name in
    let test_fun () =
      let actual = Duniverse_lib.Opam_value.to_sexp_strict input in
      Alcotest.(check (result Testable.sexp Testable.r_msg)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  let pos = ("test.opam", 1, 2) in
  [
    make_test ~name:"String" ~input:(String (pos, "a")) ~expected:(Ok (Atom "a")) ();
    make_test ~name:"List"
      ~input:(List (pos, [ String (pos, "a"); String (pos, "b") ]))
      ~expected:(Ok (List [ Atom "a"; Atom "b" ]))
      ();
    make_test ~name:"Error"
      ~input:(Bool (pos, true))
      ~expected:
        (Error (`Msg "Error in test.opam, line 1, col 2: Expected a sexp compatible opam value"))
      ();
  ]

let suite = ("Opam_value", test_to_sexp_strict)
