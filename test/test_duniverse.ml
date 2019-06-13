module Testable = struct
  module Deps = struct
    open Duniverse_lib.Duniverse.Deps

    let t = Alcotest.testable raw_pp equal

    module Source = struct
      open Source

      let t = Alcotest.testable raw_pp equal
    end

    module One = struct
      open One

      let t = Alcotest.testable raw_pp equal
    end
  end

  module R_msg = struct
    let t = Alcotest.testable Rresult.R.pp_msg (fun (`Msg s) (`Msg s') -> String.equal s s')
  end
end

let entry_factory ?(package = { Duniverse_lib.Types.Opam.name = ""; version = None })
    ?(dev_repo = `Virtual) ?tag ?(is_dune = false) () =
  { Duniverse_lib.Types.Opam.package; dev_repo; tag; is_dune }

module Deps = struct
  module Source = struct
    let test_aggregate =
      let make_test ~name ~t1 ~t2 ~expected () =
        let test_name = Printf.sprintf "Deps.Source.aggregate: %s" name in
        let test_fun () =
          let actual = Duniverse_lib.Duniverse.Deps.Source.aggregate t1 t2 in
          Alcotest.(check Testable.Deps.Source.t) test_name expected actual
        in
        (test_name, `Quick, test_fun)
      in
      [ make_test ~name:"Picks shortest name"
          ~t1:{ dir = "a"; upstream = "u"; ref = "v1" }
          ~t2:{ dir = "a-lwt"; upstream = "u"; ref = "v1" }
          ~expected:{ dir = "a"; upstream = "u"; ref = "v1" }
          ();
        make_test ~name:"Picks latest version according to opam"
          ~t1:{ dir = "a"; upstream = "u"; ref = "v1.9.0" }
          ~t2:{ dir = "a-lwt"; upstream = "u"; ref = "v1.10.0" }
          ~expected:{ dir = "a"; upstream = "u"; ref = "v1.10.0" }
          ()
      ]

    let test_aggregate_list =
      let make_test ~name ~l ~expected () =
        let test_name = Printf.sprintf "Deps.Source.aggregate_list: %s" name in
        let test_fun () =
          let actual = Duniverse_lib.Duniverse.Deps.Source.aggregate_list l in
          Alcotest.(check (list Testable.Deps.Source.t)) test_name expected actual
        in
        (test_name, `Quick, test_fun)
      in
      [ make_test ~name:"Empty" ~l:[] ~expected:[] ();
        make_test ~name:"One"
          ~l:[ { dir = "d"; upstream = "u"; ref = "r" } ]
          ~expected:[ { dir = "d"; upstream = "u"; ref = "r" } ]
          ();
        make_test ~name:"Aggregates per upstream"
          ~l:
            [ { dir = "a"; upstream = "u"; ref = "v1" };
              { dir = "a-lwt"; upstream = "u"; ref = "v1" };
              { dir = "b"; upstream = "v"; ref = "v1" }
            ]
          ~expected:
            [ { dir = "a"; upstream = "u"; ref = "v1" }; { dir = "b"; upstream = "v"; ref = "v1" } ]
          ()
      ]
  end

  module One = struct
    let test_from_opam_entry =
      let open Duniverse_lib.Duniverse.Deps.One in
      let make_test ?(get_default_branch = fun _ -> assert false) ~name ~entry ~expected () =
        let test_name = Printf.sprintf "Deps.One.from_opam_entry: %s" name in
        let test_fun () =
          let actual = from_opam_entry ~get_default_branch entry in
          Alcotest.(check (result (option Testable.Deps.One.t) Testable.R_msg.t))
            test_name expected actual
        in
        (test_name, `Quick, test_fun)
      in
      [ make_test ~name:"Virtual"
          ~entry:(entry_factory ~dev_repo:`Virtual ())
          ~expected:(Ok None) ();
        make_test ~name:"Error"
          ~entry:(entry_factory ~dev_repo:(`Error "") ())
          ~expected:(Ok None) ();
        make_test ~name:"Non dune"
          ~entry:
            (entry_factory ~dev_repo:(`Git "") ~is_dune:false
               ~package:{ name = "x"; version = Some "y" }
               ())
          ~expected:(Ok (Some (Opam { name = "x"; version = Some "y" })))
          ();
        make_test ~name:"dune"
          ~entry:
            (entry_factory ~dev_repo:(`Git "x") ~is_dune:true
               ~package:{ name = "y"; version = None } ~tag:"z" ())
          ~expected:(Ok (Some (Source { dir = "y"; upstream = "x"; ref = "z" })))
          ()
      ]
  end

  let test_from_opam_entries =
    let make_test ~name ?(get_default_branch = fun _ -> assert false) ~entries ~expected () =
      let test_name = Printf.sprintf "Deps.from_opam_entries: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Duniverse.Deps.from_opam_entries ~get_default_branch entries in
        Alcotest.(check (result Testable.Deps.t Testable.R_msg.t)) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [ make_test ~name:"Empty" ~entries:[] ~expected:(Ok { duniverse = []; opamverse = [] }) ();
      make_test ~name:"Filters virtual"
        ~entries:[ entry_factory ~dev_repo:`Virtual () ]
        ~expected:(Ok { duniverse = []; opamverse = [] })
        ();
      make_test ~name:"Filters error"
        ~entries:[ entry_factory ~dev_repo:(`Error "msg") () ]
        ~expected:(Ok { duniverse = []; opamverse = [] })
        ();
      make_test ~name:"Splits opam and source"
        ~entries:
          [ entry_factory
              ~package:{ name = "x"; version = Some "v" }
              ~dev_repo:(`Git "g") ~is_dune:false ();
            entry_factory ~package:{ name = "y"; version = None } ~tag:"w" ~dev_repo:(`Git "h")
              ~is_dune:true ()
          ]
        ~expected:
          (Ok
             { duniverse = [ { dir = "y"; upstream = "h"; ref = "w" } ];
               opamverse = [ { name = "x"; version = Some "v" } ]
             })
        ();
      make_test ~name:"Aggregates repos"
        ~entries:
          [ entry_factory ~package:{ name = "y"; version = None } ~tag:"w" ~dev_repo:(`Git "h")
              ~is_dune:true ();
            entry_factory
              ~package:{ name = "y-lwt"; version = None }
              ~tag:"w" ~dev_repo:(`Git "h") ~is_dune:true ()
          ]
        ~expected:(Ok { duniverse = [ { dir = "y"; upstream = "h"; ref = "w" } ]; opamverse = [] })
        ()
    ]
end

let suite =
  ( "Duniverse",
    Deps.One.test_from_opam_entry @ Deps.Source.test_aggregate @ Deps.Source.test_aggregate_list
    @ Deps.test_from_opam_entries )
