open Duniverse_lib

module Testable = struct
  include Testable

  module Deps = struct
    open Duniverse.Deps

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

let summary_factory ?(name = "") ?(version = "") ?dev_repo ?url_src ?(uses_dune = false) () =
  { Opam.Package_summary.name; version; dev_repo; url_src; uses_dune }

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
      [
        make_test ~name:"Picks shortest name"
          ~t:{ dir = "a"; version = "1"; dev_repo = "d"; url = Other "u"; provided_packages = [] }
          ~package:{ opam = { name = "a-lwt"; version = "1" }; dev_repo = "d"; url = Other "u" }
          ~expected:
            {
              dir = "a";
              version = "1";
              dev_repo = "d";
              url = Other "u";
              provided_packages = [ { name = "a-lwt"; version = "1" } ];
            }
          ();
        make_test ~name:"Picks latest version according to opam"
          ~t:
            {
              dir = "a";
              version = "1.9.0";
              dev_repo = "d";
              url = Other "url1";
              provided_packages = [];
            }
          ~package:
            { opam = { name = "a-lwt"; version = "1.10.0" }; dev_repo = "d"; url = Other "url2" }
          ~expected:
            {
              dir = "a";
              version = "1.10.0";
              dev_repo = "d";
              url = Other "url2";
              provided_packages = [ { name = "a-lwt"; version = "1.10.0" } ];
            }
          ();
        make_test ~name:"Adds to provided_packages no matter what"
          ~t:
            {
              dir = "a";
              version = "1.9.0";
              dev_repo = "d";
              url = Other "url";
              provided_packages = [ { name = "a"; version = "1.9.0" } ];
            }
          ~package:{ opam = { name = "a"; version = "1.10.0" }; dev_repo = "d"; url = Other "url2" }
          ~expected:
            {
              dir = "a";
              version = "1.10.0";
              dev_repo = "d";
              url = Other "url2";
              provided_packages =
                [ { name = "a"; version = "1.10.0" }; { name = "a"; version = "1.9.0" } ];
            }
          ();
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
      [
        make_test ~name:"Empty" ~l:[] ~expected:[] ();
        make_test ~name:"One"
          ~l:[ { opam = { name = "d"; version = "zdev" }; dev_repo = "d"; url = Other "u" } ]
          ~expected:
            [
              {
                dir = "d.zdev";
                version = "zdev";
                dev_repo = "d";
                url = Other "u";
                provided_packages = [ { name = "d"; version = "zdev" } ];
              };
            ]
          ();
        make_test ~name:"Aggregates per upstream"
          ~l:
            [
              { opam = { name = "a-lwt"; version = "zdev" }; dev_repo = "d"; url = Other "u" };
              { opam = { name = "a"; version = "zdev" }; dev_repo = "d"; url = Other "u" };
              { opam = { name = "b"; version = "zdev" }; dev_repo = "e"; url = Other "v" };
            ]
          ~expected:
            [
              {
                dir = "a.zdev";
                version = "zdev";
                dev_repo = "d";
                url = Other "u";
                provided_packages =
                  [ { name = "a"; version = "zdev" }; { name = "a-lwt"; version = "zdev" } ];
              };
              {
                dir = "b.zdev";
                version = "zdev";
                dev_repo = "e";
                url = Other "v";
                provided_packages = [ { name = "b"; version = "zdev" } ];
              };
            ]
          ();
      ]
  end

  module Classified = struct
    let test_from_opam_entry =
      let open Duniverse_lib.Duniverse.Deps.Classified in
      let make_test ?(get_default_branch = fun _ -> assert false) ~name ~summary ~expected () =
        let test_name = Printf.sprintf "Deps.Classified.from_opam_entry: %s" name in
        let test_fun () =
          let actual = from_package_summary ~get_default_branch summary in
          Alcotest.(check (result (option Testable.Deps.Classified.t) Testable.r_msg))
            test_name expected actual
        in
        (test_name, `Quick, test_fun)
      in
      [
        make_test ~name:"No url_src"
          ~summary:(summary_factory ?url_src:None ())
          ~expected:(Ok None) ();
        make_test ~name:"No dev_repo"
          ~summary:(summary_factory ?dev_repo:None ())
          ~expected:(Ok None) ();
        make_test ~name:"Non dune"
          ~summary:
            (summary_factory ~dev_repo:"d" ~url_src:(Other "u") ~uses_dune:false ~name:"x"
               ~version:"v" ())
          ~expected:(Ok (Some (Opam { name = "x"; version = "v" })))
          ();
        make_test ~name:"dune"
          ~summary:
            (summary_factory ~dev_repo:"d" ~url_src:(Other "u") ~uses_dune:true ~name:"y"
               ~version:"v" ())
          ~expected:
            (Ok
               (Some
                  (Source { opam = { name = "y"; version = "v" }; dev_repo = "d"; url = Other "u" })))
          ();
        make_test ~name:"Uses default branch when no tag"
          ~get_default_branch:(function "r" -> Ok "master" | _ -> assert false)
          ~summary:
            (summary_factory ~dev_repo:"d"
               ~url_src:(Git { repo = "r"; ref = None })
               ~uses_dune:true ~name:"y" ~version:"v" ())
          ~expected:
            (Ok
               (Some
                  (Source
                     {
                       opam = { name = "y"; version = "v" };
                       dev_repo = "d";
                       url = Git { repo = "r"; ref = "master" };
                     })))
          ();
      ]
  end

  let test_from_opam_entries =
    let make_test ~name ?(get_default_branch = fun _ -> assert false) ~entries ~expected () =
      let test_name = Printf.sprintf "Deps.from_opam_entries: %s" name in
      let test_fun () =
        let actual =
          Duniverse_lib.Duniverse.Deps.from_package_summaries ~get_default_branch entries
        in
        Alcotest.(check (result Testable.Deps.unresolved Testable.r_msg)) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Empty" ~entries:[] ~expected:(Ok { duniverse = []; opamverse = [] }) ();
      make_test ~name:"Filters virtual"
        ~entries:[ summary_factory ?dev_repo:None () ]
        ~expected:(Ok { duniverse = []; opamverse = [] })
        ();
      make_test ~name:"Filters base packages"
        ~entries:[ summary_factory ~dev_repo:"d" ~url_src:(Other "u") ~name:"dune" () ]
        ~expected:(Ok { duniverse = []; opamverse = [] })
        ();
      make_test ~name:"Splits opam and source"
        ~entries:
          [
            summary_factory ~name:"x" ~version:"v" ~url_src:(Other "u") ~dev_repo:"d"
              ~uses_dune:false ();
            summary_factory ~name:"y" ~version:"w" ~url_src:(Other "v") ~dev_repo:"e"
              ~uses_dune:true ();
          ]
        ~expected:
          (Ok
             {
               duniverse =
                 [
                   {
                     dir = "y.w";
                     version = "w";
                     dev_repo = "e";
                     url = Other "v";
                     provided_packages = [ { name = "y"; version = "w" } ];
                   };
                 ];
               opamverse = [ { name = "x"; version = "v" } ];
             })
        ();
      make_test ~name:"Aggregates repos"
        ~entries:
          [
            summary_factory ~name:"y" ~version:"v" ~url_src:(Other "u") ~dev_repo:"d"
              ~uses_dune:true ();
            summary_factory ~name:"y-lwt" ~version:"v" ~url_src:(Other "u") ~dev_repo:"d"
              ~uses_dune:true ();
          ]
        ~expected:
          (Ok
             {
               duniverse =
                 [
                   {
                     dir = "y-lwt.v";
                     version = "v";
                     dev_repo = "d";
                     url = Other "u";
                     provided_packages =
                       [ { name = "y-lwt"; version = "v" }; { name = "y"; version = "v" } ];
                   };
                 ];
               opamverse = [];
             })
        ();
    ]
end

let suite =
  ( "Duniverse",
    Deps.Classified.test_from_opam_entry @ Deps.Source.test_aggregate
    @ Deps.Source.test_aggregate_list @ Deps.test_from_opam_entries )
