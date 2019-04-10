let test_tag_from_archive =
  let make_test ~archive ~expected =
    let test_name = "tag_from_archive: " ^ archive in
    let test_fun () =
      let actual = Duniverse_lib.Opam_cmd.tag_from_archive archive in 
      Alcotest.(check (option string)) archive expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test ~archive:"git+http://a.com/user/repo" ~expected:(Some "master")
  ; make_test ~archive:"git+https://a.com/user/repo" ~expected:(Some "master")
  ; make_test ~archive:"git+https://a.com/user/repo#v0.0.0" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"git+ssh://a.com/user/repo#v0.0.0" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"git+file://a.com/user/repo/something" ~expected:None
  ; make_test ~archive:"https://github.com/user/repo/releases/download/v0.0.0/archive.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://github.com/user/repo/archive/v0.0.0.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://github.com/user/repo/archive/v0.0.0/archive.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://ocaml.janestreet.com/ocaml-core/4.07.1/files/package-v0.0.0.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://ocaml.janestreet.com/janestreet/repo/releases/download/v0.0.0/file.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://ocaml.janestreet.com/janestreet/repo/archive/v0.0.0.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://ocaml.janestreet.com/janestreet/malformed" ~expected:None
  ; make_test ~archive:"https://gitlab.camlcity.org/some/path/file-v0.0.0.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://download.camlcity.org/some/path/file-v0.0.0.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://ocamlgraph.lri.fr/some/path/file-0.0.0.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://erratique.ch/some/path/file-0.0.0.tbz" ~expected:(Some "v0.0.0")
  ; make_test ~archive:"https://other.domain.com/some/path/file-v0.0.0.tbz" ~expected:(Some "v0.0.0")
  ]
