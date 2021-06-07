module Testable = struct
  module Url = struct
    open Duniverse_lib.Opam.Url

    let t = Alcotest.testable pp equal
  end
end

module Url = struct
  let test_from_opam =
    let make_test ~url_src ~expected () =
      let test_name = Printf.sprintf "Url.from_opam: %s" url_src in
      let test_fun () =
        let url = OpamUrl.parse url_src in
        let actual = Duniverse_lib.Opam.Url.from_opam url in
        Alcotest.(check Testable.Url.t) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~url_src:"https://some/archive.tbz" ~expected:(Other "https://some/archive.tbz") ();
      make_test ~url_src:"hg+https://some/repo" ~expected:(Other "hg+https://some/repo") ();
      make_test ~url_src:"file:///home/user/repo" ~expected:(Other "file:///home/user/repo") ();
      make_test ~url_src:"git+https://some/repo"
        ~expected:(Git { repo = "git+https://some/repo"; ref = None })
        ();
      make_test ~url_src:"git://some/repo"
        ~expected:(Git { repo = "git://some/repo"; ref = None })
        ();
      make_test ~url_src:"https://some/repo.git"
        ~expected:(Git { repo = "git+https://some/repo.git"; ref = None })
        ();
      make_test ~url_src:"git+https://some/repo.git#ref"
        ~expected:(Git { repo = "git+https://some/repo.git"; ref = Some "ref" })
        ();
    ]
end

let opam_parse_formula s =
  let pp = OpamFormat.V.package_formula `Conj OpamFormat.V.(filtered_constraints ext_version) in
  let pos = ("", 0, 0) in
  OpamPp.parse ~pos pp (OpamParser.value_from_string s "_" [@alert "-deprecated"])

let test_depends_on_dune =
  let make_test ~name ~allow_jbuilder ~input ~expected () =
    let test_name = Printf.sprintf "depends_on_dune: %s" name in
    let test_fun () =
      let formula = opam_parse_formula input in
      let actual = Duniverse_lib.Opam.depends_on_dune ~allow_jbuilder formula in
      Alcotest.(check bool) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"No deps" ~allow_jbuilder:false ~input:"[]" ~expected:false ();
    make_test ~name:"Just dune" ~allow_jbuilder:false ~input:{|["dune"]|} ~expected:true ();
    make_test ~name:"Versioned dune" ~allow_jbuilder:false ~input:{|["dune" {>= "2.1"}]|}
      ~expected:true ();
    make_test ~name:"jbuilder disallowed" ~allow_jbuilder:false ~input:{|["jbuilder"]|}
      ~expected:false ();
    make_test ~name:"jbuilder allowed" ~allow_jbuilder:true ~input:{|["jbuilder"]|} ~expected:true
      ();
    make_test ~name:"Several deps" ~allow_jbuilder:false
      ~input:{|["ocaml" {>= "4.08"} "fmt" {>= "0.8.4"} "dune" {>= "2.1"} "logs" {>= "0.4"}]|}
      ~expected:true ();
  ]

let test_depends_on_compiler_variants =
  let make_test ~name ~input ~expected () =
    let test_name = Printf.sprintf "depends_on_compiler_variants: %s" name in
    let test_fun () =
      let formula = opam_parse_formula input in
      let actual = Duniverse_lib.Opam.depends_on_compiler_variants formula in
      Alcotest.(check bool) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  [
    make_test ~name:"Depends on ocaml" ~input:{|[ "ocaml" {>= "4.11"} ]|} ~expected:false ();
    make_test ~name:"Depends variant directly"
      ~input:{|[ "ocaml-variants" {= "4.11.1+flambda+afl"} ]|} ~expected:true ();
  ]

let suite =
  ( "Opam",
    List.concat [ Url.test_from_opam; test_depends_on_dune; test_depends_on_compiler_variants ] )
