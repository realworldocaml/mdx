open Duniverse_lib

module Testable = struct
  include Testable
  open Duniverse

  module Repo = struct
    open Repo

    let unresolved = Alcotest.testable (pp Git.Ref.pp) (equal Git.Ref.equal)

    module Package = struct
      open Package

      let t = Alcotest.testable pp equal
    end
  end
end

let summary_factory ?(name = "") ?(version = "") ?dev_repo ?url_src ?(hashes = []) () =
  { Opam.Package_summary.name; version; dev_repo; url_src; hashes }

module Repo = struct
  module Package = struct
    let test_from_package_summary =
      let open Duniverse.Repo.Package in
      let make_test ?(get_default_branch = fun _ -> assert false) ~name ~summary ~expected () =
        let test_name = Printf.sprintf "Repo.Package.from_package_summary: %s" name in
        let test_fun () =
          let actual = from_package_summary ~get_default_branch summary in
          Alcotest.(check (result (option Testable.Repo.Package.t) Testable.r_msg))
            test_name expected actual
        in
        (test_name, `Quick, test_fun)
      in
      [
        make_test ~name:"Base package" ~summary:(summary_factory ~name:"dune" ())
          ~expected:(Ok None) ();
        make_test ~name:"No url_src"
          ~summary:(summary_factory ?url_src:None ())
          ~expected:(Ok None) ();
        make_test ~name:"No dev_repo"
          ~summary:(summary_factory ?dev_repo:None ())
          ~expected:(Ok None) ();
        make_test ~name:"Regular"
          ~summary:
            (summary_factory ~dev_repo:"d" ~url_src:(Other "u") ~name:"y" ~version:"v" ~hashes:[]
               ())
          ~expected:
            (Ok
               (Some
                  {
                    opam = { name = "y"; version = "v" };
                    dev_repo = "d";
                    url = Other "u";
                    hashes = [];
                  }))
          ();
        make_test ~name:"Uses default branch when no tag"
          ~get_default_branch:(function "r" -> Ok "master" | _ -> assert false)
          ~summary:
            (summary_factory ~dev_repo:"d"
               ~url_src:(Git { repo = "r"; ref = None })
               ~name:"y" ~version:"v" ~hashes:[] ())
          ~expected:
            (Ok
               (Some
                  {
                    opam = { name = "y"; version = "v" };
                    dev_repo = "d";
                    url = Git { repo = "r"; ref = "master" };
                    hashes = [];
                  }))
          ();
      ]
  end

  let package_factory ?(name = "") ?(version = "") ?(dev_repo = "")
      ?(url = Duniverse.Repo.Url.Other "") ?(hashes = []) () =
    let open Duniverse.Repo.Package in
    { opam = { name; version }; dev_repo; url; hashes }

  let test_from_packages =
    let make_test ~name ~dev_repo ~packages ~expected () =
      let test_name = Printf.sprintf "Repo.from_packages: %s" name in
      let test_fun () =
        let dev_repo = Dev_repo.from_string dev_repo in
        let actual = Duniverse_lib.Duniverse.Repo.from_packages ~dev_repo packages in
        Alcotest.(check Testable.Repo.unresolved) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Simple" ~dev_repo:"d"
        ~packages:[ package_factory ~name:"p" ~version:"v" ~url:(Other "u") ~hashes:[] () ]
        ~expected:
          {
            dir = "d";
            url = Other "u";
            hashes = [];
            provided_packages = [ { name = "p"; version = "v" } ];
          }
        ();
      make_test ~name:"Uses repository name as dir" ~dev_repo:"https://github.com/org/repo.git"
        ~packages:[ package_factory ~name:"p" ~version:"v" ~url:(Other "u") ~hashes:[] () ]
        ~expected:
          {
            dir = "repo";
            url = Other "u";
            hashes = [];
            provided_packages = [ { name = "p"; version = "v" } ];
          }
        ();
      make_test ~name:"Expection for dune" ~dev_repo:"https://github.com/ocaml/dune.git"
        ~packages:[ package_factory ~name:"p" ~version:"v" ~url:(Other "u") ~hashes:[] () ]
        ~expected:
          {
            dir = "dune_";
            url = Other "u";
            hashes = [];
            provided_packages = [ { name = "p"; version = "v" } ];
          }
        ();
      make_test ~name:"Add all to provided packages" ~dev_repo:"d"
        ~packages:
          [
            package_factory ~name:"d" ~version:"zdev" ~url:(Other "u") ~hashes:[] ();
            package_factory ~name:"d-lwt" ~version:"zdev" ~url:(Other "u") ~hashes:[] ();
          ]
        ~expected:
          {
            dir = "d";
            url = Other "u";
            hashes = [];
            provided_packages =
              [ { name = "d"; version = "zdev" }; { name = "d-lwt"; version = "zdev" } ];
          }
        ();
      make_test ~name:"Pick URL from highest version package" ~dev_repo:"d"
        ~packages:
          [
            package_factory ~name:"d" ~version:"1" ~url:(Other "u1") ~hashes:[] ();
            package_factory ~name:"d-lwt" ~version:"2" ~url:(Other "u2") ~hashes:[] ();
          ]
        ~expected:
          {
            dir = "d";
            url = Other "u2";
            hashes = [];
            provided_packages = [ { name = "d"; version = "1" }; { name = "d-lwt"; version = "2" } ];
          }
        ();
    ]
end

let test_from_package_summaries =
  let make_test ~name ?(get_default_branch = fun _ -> assert false) ~summaries ~expected () =
    let test_name = Printf.sprintf "from_package_summaries: %s" name in
    let test_fun () =
      let actual = Duniverse_lib.Duniverse.from_package_summaries ~get_default_branch summaries in
      Alcotest.(check (result (list Testable.Repo.unresolved) Testable.r_msg))
        test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"Empty" ~summaries:[] ~expected:(Ok []) ();
    make_test ~name:"Filters virtual"
      ~summaries:[ summary_factory ?dev_repo:None () ]
      ~expected:(Ok []) ();
    make_test ~name:"Filters base packages"
      ~summaries:[ summary_factory ~dev_repo:"d" ~url_src:(Other "u") ~name:"dune" () ]
      ~expected:(Ok []) ();
    make_test ~name:"Simple"
      ~summaries:
        [ summary_factory ~name:"x" ~version:"v" ~url_src:(Other "u") ~dev_repo:"d" ~hashes:[] () ]
      ~expected:
        (Ok
           [
             {
               dir = "d";
               url = Other "u";
               hashes = [];
               provided_packages = [ { name = "x"; version = "v" } ];
             };
           ])
      ();
    make_test ~name:"Aggregates repos"
      ~summaries:
        [
          summary_factory ~name:"y" ~version:"v" ~url_src:(Other "u") ~dev_repo:"d" ~hashes:[] ();
          summary_factory ~name:"y-lwt" ~version:"v" ~url_src:(Other "u") ~dev_repo:"d" ~hashes:[]
            ();
        ]
      ~expected:
        (Ok
           [
             {
               dir = "d";
               url = Other "u";
               hashes = [];
               provided_packages =
                 [ { name = "y-lwt"; version = "v" }; { name = "y"; version = "v" } ];
             };
           ])
      ();
  ]

let suite =
  ( "Duniverse",
    List.concat
      [
        Repo.Package.test_from_package_summary; Repo.test_from_packages; test_from_package_summaries;
      ] )
