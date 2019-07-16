module Testable = struct
  open Duniverse_lib.Types.Opam

  let opam_repo = Alcotest.testable pp_repo equal_repo
end

let test_tag_from_archive =
  let make_test ?name ~archive ~expected () =
    let name = match name with Some n -> n | None -> archive in
    let test_name = "tag_from_archive: " ^ name in
    let test_fun () =
      let actual = Duniverse_lib.Opam_cmd.tag_from_archive archive in
      Alcotest.(check (option string)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~name:"empty" ~archive:"" ~expected:None ();
    make_test ~archive:"malformed" ~expected:None ();
    make_test ~archive:"git+http://a.com/user/repo" ~expected:(Some "master") ();
    make_test ~archive:"git+https://a.com/user/repo" ~expected:(Some "master") ();
    make_test ~archive:"git+https://a.com/user/repo#v1.2.3" ~expected:(Some "v1.2.3") ();
    make_test ~archive:"git+https://github.com/user/repo#v1.2.3" ~expected:(Some "v1.2.3") ();
    make_test ~archive:"git+ssh://a.com/user/repo#v1.2.3" ~expected:(Some "v1.2.3") ();
    make_test ~archive:"git+file://a.com/user/repo/something" ~expected:None ();
    make_test ~archive:"https://github.com/user/repo/releases/download/v1.2.3/archive.tbz"
      ~expected:(Some "v1.2.3") ();
    make_test ~archive:"https://github.com/user/repo/archive/v1.2.3.tbz" ~expected:(Some "v1.2.3")
      ();
    make_test ~archive:"https://github.com/user/repo/archive/v1.2.3/archive.tbz"
      ~expected:(Some "v1.2.3") ();
    make_test ~archive:"https://ocaml.janestreet.com/ocaml-core/4.07.1/files/package-v1.2.3.tbz"
      ~expected:(Some "v1.2.3") ();
    make_test
      ~archive:"https://ocaml.janestreet.com/janestreet/repo/releases/download/v1.2.3/file.tbz"
      ~expected:(Some "v1.2.3") ();
    make_test ~archive:"https://ocaml.janestreet.com/janestreet/repo/archive/v1.2.3.tbz"
      ~expected:(Some "v1.2.3") ();
    make_test ~archive:"https://ocaml.janestreet.com/janestreet/malformed" ~expected:None ();
    make_test ~archive:"https://gitlab.camlcity.org/some/path/file-v1.2.3.tbz"
      ~expected:(Some "v1.2.3") ();
    make_test ~archive:"https://download.camlcity.org/some/path/file-v1.2.3.tbz"
      ~expected:(Some "v1.2.3") ();
    make_test ~archive:"https://ocamlgraph.lri.fr/some/path/file-1.2.3.tbz"
      ~expected:(Some "v1.2.3") ();
    make_test ~archive:"https://erratique.ch/some/path/file-1.2.3.tbz" ~expected:(Some "v1.2.3") ();
    make_test ~archive:"https://other.domain.com/some/path/file-v1.2.3.tbz"
      ~expected:(Some "v1.2.3") ()
  ]

let test_classify_package =
  let make_test ~name ~package ?(dev_repo = "dummy-dev-repo") ?archive ~expected () =
    let test_name = Printf.sprintf "classify_package: %s" name in
    let test_fun () =
      let actual = Duniverse_lib.Opam_cmd.classify_package ~package ~dev_repo ~archive () in
      Alcotest.(check (pair Testable.opam_repo (option string))) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~name:"base package"
      ~package:{ name = "ocaml"; version = None }
      ~expected:(`Virtual, None)
      ();
    make_test ~name:"base package versioned"
      ~package:{ name = "ocaml"; version = Some "1" }
      ~expected:(`Virtual, None)
      ();
    make_test ~name:"empty dev-repo" ~package:{ name = "x"; version = None } ~dev_repo:""
      ~expected:(`Virtual, None)
      ();
    make_test ~name:"no archive" ~package:{ name = "x"; version = None }
      ~dev_repo:"host.com/path.git" ?archive:None
      ~expected:(`Virtual, None)
      ();
    make_test ~name:"github dev-repo" ~package:{ name = "x"; version = None }
      ~dev_repo:"git+https://github.com/user/repo.git" ~archive:""
      ~expected:(`Git "https://github.com/user/repo.git", None)
      ();
    make_test ~name:"guess tag from archive" ~package:{ name = "x"; version = None }
      ~dev_repo:"git+https://github.com/user/repo.git" ~archive:"file-v1.tbz"
      ~expected:(`Git "https://github.com/user/repo.git", Some "v1")
      ();
    make_test ~name:"no host" ~package:{ name = "x"; version = None } ~dev_repo:"nohost.git"
      ~archive:""
      ~expected:(`Error "dev-repo without host", None)
      ();
    make_test ~name:"git" ~package:{ name = "x"; version = None }
      ~dev_repo:"git+https://host.com/some-repo.git" ~archive:"gitpaf#pouf"
      ~expected:(`Git "https://host.com/some-repo.git", None)
      ();
    make_test ~name:"wrong vcs" ~package:{ name = "x"; version = None }
      ~dev_repo:"hg+https://host.com/some-repo" ~archive:""
      ~expected:(`Error "dev-repo doesn't use git as a VCS", None)
      ()
  ]

let suite = ("Opam_cmd", test_tag_from_archive @ test_classify_package)
