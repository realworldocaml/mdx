module Testable = struct
  module Dev_repo = struct
    open Duniverse_lib.Opam.Dev_repo

    let t = Alcotest.testable pp equal
  end

  open Duniverse_lib.Types.Opam

  let opam_repo = Alcotest.testable pp_repo equal_repo
end

module Dev_repo = struct
  let test_from_string =
    let make_test ~dev_repo ~expected () =
      let test_name = Printf.sprintf "Dev_repo.from_string: %s" dev_repo in
      let test_fun () =
        let actual = Duniverse_lib.Opam.Dev_repo.from_string dev_repo in
        Alcotest.(check Testable.Dev_repo.t) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~dev_repo:"https://host.com/repo"
        ~expected:{ vcs = None; uri = Uri.of_string "https://host.com/repo" }
        ();
      make_test ~dev_repo:"https://host.com/repo.git"
        ~expected:{ vcs = Some Git; uri = Uri.of_string "https://host.com/repo.git" }
        ();
      make_test ~dev_repo:"git+https://host.com/repo.git"
        ~expected:{ vcs = Some Git; uri = Uri.of_string "https://host.com/repo.git" }
        ();
      make_test ~dev_repo:"hg+https://host.com/repo"
        ~expected:{ vcs = Some (Other "hg"); uri = Uri.of_string "https://host.com/repo" }
        ();
      make_test ~dev_repo:"git://github.com/lpw25/async_graphics"
        ~expected:{ vcs = Some Git; uri = Uri.of_string "git://github.com/lpw25/async_graphics" }
        ();
    ]
end

let test_tag_from_archive =
  let make_test ?name ~archive ~expected () =
    let name = match name with Some n -> n | None -> archive in
    let test_name = "tag_from_archive: " ^ name in
    let test_fun () =
      let actual = Duniverse_lib.Opam.tag_from_archive archive in
      Alcotest.(check (option string)) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"empty" ~archive:"" ~expected:None ();
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
      ~expected:(Some "v1.2.3") ();
    make_test
      ~archive:"https://ocaml.janestreet.com/ocaml-core/109.32.00/individual/async_core-109.32.00.tar.gz"
      ~expected:(Some "109.32.00") ();
  ]

let test_classify_package =
  let make_test ~name ~package ?(dev_repo = Some "dummy-dev-repo") ?archive ~expected () =
    let test_name = Printf.sprintf "classify_package: %s" name in
    let test_fun () =
      let actual = Duniverse_lib.Opam.classify_package ~package ~dev_repo ~archive () in
      Alcotest.(check (pair Testable.opam_repo (option string))) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"base package"
      ~package:{ name = "ocaml"; version = None }
      ~expected:(`Virtual, None)
      ();
    make_test ~name:"base package versioned"
      ~package:{ name = "ocaml"; version = Some "1" }
      ~expected:(`Virtual, None)
      ();
    make_test ~name:"empty dev-repo" ~package:{ name = "x"; version = None } ~dev_repo:None
      ~expected:(`Virtual, None)
      ();
    make_test ~name:"no archive" ~package:{ name = "x"; version = None }
      ~dev_repo:(Some "host.com/path.git") ?archive:None
      ~expected:(`Virtual, None)
      ();
    make_test ~name:"github dev-repo" ~package:{ name = "x"; version = None }
      ~dev_repo:(Some "git+https://github.com/user/repo.git") ~archive:""
      ~expected:(`Git "https://github.com/user/repo.git", None)
      ();
    make_test ~name:"guess tag from archive" ~package:{ name = "x"; version = None }
      ~dev_repo:(Some "git+https://github.com/user/repo.git") ~archive:"file-v1.tbz"
      ~expected:(`Git "https://github.com/user/repo.git", Some "v1")
      ();
    make_test ~name:"no host" ~package:{ name = "x"; version = None } ~dev_repo:(Some "nohost.git")
      ~archive:""
      ~expected:(`Error "dev-repo without host", None)
      ();
    make_test ~name:"git" ~package:{ name = "x"; version = None }
      ~dev_repo:(Some "git+https://host.com/some-repo.git") ~archive:"gitpaf#pouf"
      ~expected:(`Git "https://host.com/some-repo.git", None)
      ();
    make_test ~name:"wrong vcs" ~package:{ name = "x"; version = None }
      ~dev_repo:(Some "hg+https://host.com/some-repo") ~archive:""
      ~expected:(`Error "dev-repo doesn't use git as a VCS", None)
      ();
    make_test ~name:"use url.src when possible" ~package:{ name = "x"; version = None }
      ~dev_repo:(Some "git+https://host.com/some-repo.git")
      ~archive:"git+https://host.com/some-fork.git#dev"
      ~expected:(`Git "https://host.com/some-fork.git", Some "dev")
      ();
    make_test ~name:"fallback to dev_repo" ~package:{ name = "x"; version = None }
      ~dev_repo:(Some "git+https://host.com/some-repo.git")
      ~archive:"https://github.com/user/repo/releases/download/v1.2.3/archive.tbz"
      ~expected:(`Git "https://host.com/some-repo.git", Some "v1.2.3")
      ();
  ]

let suite = ("Opam", Dev_repo.test_from_string @ test_tag_from_archive @ test_classify_package)
