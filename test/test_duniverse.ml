module Testable = struct
  include Testable

  module Deps = struct
    open Duniverse_lib
    open Duniverse_lib.Duniverse.Deps

    let unresolved = Alcotest.testable (raw_pp Git.Ref.pp) (equal Git.Ref.equal)

    module Source = struct
      open Source

      let unresolved = Alcotest.testable (raw_pp Git.Ref.pp) (equal Git.Ref.equal)
    end

    module Classified = struct
      open Classified

      let t = Alcotest.testable raw_pp equal
    end
  end
end

let entry_factory ?(package = { Duniverse_lib.Types.Opam.name = ""; version = None })
    ?(dev_repo = `Virtual) ?tag ?(is_dune = false) () =
  { Duniverse_lib.Types.Opam.package; dev_repo; tag; is_dune }

module Deps = struct
  module Source = struct
    let test_aggregate =
      let make_test ~name ~t ~package ~expected () =
        let test_name = Printf.sprintf "Deps.Source.aggregate: %s" name in
        let test_fun () =
          let actual = Duniverse_lib.Duniverse.Deps.Source.aggregate t package in
          Alcotest.(check Testable.Deps.Source.unresolved) test_name expected actual
        in
        (test_name, `Quick, test_fun)
      in
      [ make_test ~name:"Picks shortest name"
          ~t:{ dir = "a"; upstream = "u"; ref = "v1"; provided_packages = [] }
          ~package:{ opam = { name = "a-lwt"; version = None }; upstream = "u"; ref = "v1" }
          ~expected:
            { dir = "a";
              upstream = "u";
              ref = "v1";
              provided_packages = [ { name = "a-lwt"; version = None } ]
            }
          ();
        make_test ~name:"Picks latest version according to opam"
          ~t:{ dir = "a"; upstream = "u"; ref = "v1.9.0"; provided_packages = [] }
          ~package:{ opam = { name = "a-lwt"; version = None }; upstream = "u"; ref = "v1.10.0" }
          ~expected:
            { dir = "a";
              upstream = "u";
              ref = "v1.10.0";
              provided_packages = [ { name = "a-lwt"; version = None } ]
            }
          ();
        make_test ~name:"Adds to provided_packages no matter what"
          ~t:
            { dir = "a";
              upstream = "u";
              ref = "v1.9.0";
              provided_packages = [ { name = "a"; version = Some "1.9.0" } ]
            }
          ~package:
            { opam = { name = "a"; version = Some "1.10.0" }; upstream = "u"; ref = "v1.10.0" }
          ~expected:
            { dir = "a";
              upstream = "u";
              ref = "v1.10.0";
              provided_packages =
                [ { name = "a"; version = Some "1.10.0" }; { name = "a"; version = Some "1.9.0" } ]
            }
          ()
      ]

    let test_aggregate_list =
      let make_test ~name ~l ~expected () =
        let test_name = Printf.sprintf "Deps.Source.aggregate_list: %s" name in
        let test_fun () =
          let actual = Duniverse_lib.Duniverse.Deps.Source.aggregate_packages l in
          Alcotest.(check (list Testable.Deps.Source.unresolved)) test_name expected actual
        in
        (test_name, `Quick, test_fun)
      in
      [ make_test ~name:"Empty" ~l:[] ~expected:[] ();
        make_test ~name:"One"
          ~l:[ { opam = { name = "d"; version = None }; upstream = "u"; ref = "r" } ]
          ~expected:
            [ { dir = "d.zdev";
                upstream = "u";
                ref = "r";
                provided_packages = [ { name = "d"; version = None } ]
              }
            ]
          ();
        make_test ~name:"Aggregates per upstream"
          ~l:
            [ { opam = { name = "a-lwt"; version = None }; upstream = "u"; ref = "v1" };
              { opam = { name = "a"; version = None }; upstream = "u"; ref = "v1" };
              { opam = { name = "b"; version = None }; upstream = "v"; ref = "v1" }
            ]
          ~expected:
            [ { dir = "a.zdev";
                upstream = "u";
                ref = "v1";
                provided_packages =
                  [ { name = "a"; version = None }; { name = "a-lwt"; version = None } ]
              };
              { dir = "b.zdev";
                upstream = "v";
                ref = "v1";
                provided_packages = [ { name = "b"; version = None } ]
              }
            ]
          ()
      ]
  end

  module Classified = struct
    let test_from_opam_entry =
      let open Duniverse_lib.Duniverse.Deps.Classified in
      let make_test ?(get_default_branch = fun _ -> assert false) ~name ~entry ~expected () =
        let test_name = Printf.sprintf "Deps.Classified.from_opam_entry: %s" name in
        let test_fun () =
          let actual = from_opam_entry ~get_default_branch entry in
          Alcotest.(check (result (option Testable.Deps.Classified.t) Testable.r_msg))
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
          ~expected:
            (Ok
               (Some (Source { opam = { name = "y"; version = None }; upstream = "x"; ref = "z" })))
          ();
        make_test ~name:"Uses default branch when no tag"
          ~get_default_branch:(function "x" -> Ok "z" | _ -> assert false)
          ~entry:
            (entry_factory ~dev_repo:(`Git "x") ~is_dune:true
               ~package:{ name = "y"; version = None } ?tag:None ())
          ~expected:
            (Ok
               (Some (Source { opam = { name = "y"; version = None }; upstream = "x"; ref = "z" })))
          ()
      ]
  end

  let test_from_opam_entries =
    let make_test ~name ?(get_default_branch = fun _ -> assert false) ~entries ~expected () =
      let test_name = Printf.sprintf "Deps.from_opam_entries: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Duniverse.Deps.from_opam_entries ~get_default_branch entries in
        Alcotest.(check (result Testable.Deps.unresolved Testable.r_msg)) test_name expected actual
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
             { duniverse =
                 [ { dir = "y.zdev";
                     upstream = "h";
                     ref = "w";
                     provided_packages = [ { name = "y"; version = None } ]
                   }
                 ];
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
        ~expected:
          (Ok
             { duniverse =
                 [ { dir = "y-lwt.zdev";
                     upstream = "h";
                     ref = "w";
                     provided_packages =
                       [ { name = "y-lwt"; version = None }; { name = "y"; version = None } ]
                   }
                 ];
               opamverse = []
             })
        ()
    ]
end

let suite =
  ( "Duniverse",
    Deps.Classified.test_from_opam_entry @ Deps.Source.test_aggregate
    @ Deps.Source.test_aggregate_list @ Deps.test_from_opam_entries )
