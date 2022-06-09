open Duniverse_lib
open Import

let opam_url_set =
  let pp fmt url_set =
    let elements = OpamUrl.Set.elements url_set in
    Fmt.pf fmt "[ %a ]"
      Fmt.(list ~sep:(any ";@ ") Duniverse_lib.Opam.Pp.url)
      elements
  in
  Alcotest.testable pp OpamUrl.Set.equal

let variable_content_string_map =
  let equal = String.Map.equal ~cmp:( = ) in
  let pp fmt map =
    let bindings = String.Map.bindings map in
    let pp_one fmt (name, value) =
      Fmt.pf fmt "(%s : %s)" name
        (OpamVariable.string_of_variable_contents value)
    in
    Fmt.pf fmt "[ %a ]" Fmt.(list ~sep:(any ";@ ") pp_one) bindings
  in
  Alcotest.testable pp equal

module Opam_repositories_url_rewriter = struct
  let test_rewrite_one_in =
    let make_test ~name ~opam_monorepo_cwd ~value ~expected =
      let test_name =
        Printf.sprintf "Opam_repositories_url_rewriter.rewrite_one_in: %s" name
      in
      let test_fun () =
        let open Source_opam_config.Private in
        let value = OpamUrl.of_string value in
        let actual =
          Opam_repositories_url_rewriter.rewrite_one_in ~opam_monorepo_cwd value
          |> Result.map ~f:OpamUrl.to_string
        in
        Alcotest.(check (result string Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Rewrites root" ~opam_monorepo_cwd:"/home/a"
        ~value:"file://$OPAM_MONOREPO_CWD/something_local"
        ~expected:(Ok "file:///home/a/something_local");
      make_test ~name:"Rewrites vcs root" ~opam_monorepo_cwd:"/home/a"
        ~value:"git+file://$OPAM_MONOREPO_CWD/something_local"
        ~expected:(Ok "git+file:///home/a/something_local");
      make_test ~name:"Does not rewrite remote" ~opam_monorepo_cwd:"/home/a"
        ~value:"https://$OPAM_MONOREPO_CWD/something_local"
        ~expected:
          (Rresult.R.error_msg
             "$OPAM_MONOREPO_CWD can only be used to rewrite the root part of \
              file:// URLs. \"https://$OPAM_MONOREPO_CWD/something_local\" is \
              an invalid use of the variable.");
      make_test ~name:"Does not rewrite non root" ~opam_monorepo_cwd:"/home/a"
        ~value:"file:///home/$OPAM_MONOREPO_CWD/something_local"
        ~expected:
          (Rresult.R.error_msg
             "$OPAM_MONOREPO_CWD can only be used to rewrite the root part of \
              file:// URLs. \
              \"file:///home/$OPAM_MONOREPO_CWD/something_local\" is an \
              invalid use of the variable.");
    ]

  let test_rewrite_one_out =
    let make_test ~name ~opam_monorepo_cwd ~expected ~value =
      let test_name =
        Printf.sprintf "Opam_repositories_url_rewriter.rewrite_one_out: %s" name
      in
      let test_fun () =
        let open Source_opam_config.Private in
        let value = OpamUrl.of_string value in
        let actual =
          Opam_repositories_url_rewriter.rewrite_one_out ~opam_monorepo_cwd
            value
          |> OpamUrl.to_string
        in
        Alcotest.(check string) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Rewrites root" ~value:"file:///home/a/something_local"
        ~opam_monorepo_cwd:"/home/a"
        ~expected:"file://$OPAM_MONOREPO_CWD/something_local";
      make_test ~name:"Rewrites vcs root"
        ~value:"git+file:///home/a/something_local" ~opam_monorepo_cwd:"/home/a"
        ~expected:"git+file://$OPAM_MONOREPO_CWD/something_local";
      make_test ~name:"Does not rewrite remote"
        ~value:"https:///home/a/something.com" ~opam_monorepo_cwd:"/home/a"
        ~expected:"https:///home/a/something.com";
      make_test ~name:"Does not rewrite non root"
        ~value:"file:///house/home/a/something" ~opam_monorepo_cwd:"/home/a"
        ~expected:"file:///house/home/a/something";
    ]
end

module Opam_repositories = struct
  let t_from_input l = OpamUrl.Set.of_list (List.map ~f:OpamUrl.of_string l)

  let test_from_opam_value =
    let make_test ~name ~value ~expected () =
      let test_name =
        Printf.sprintf "Opam_repositories.from_opam_value: %s" name
      in
      let test_fun () =
        let value = OpamParser.FullPos.value_from_string value "test.opam" in
        let expected = Result.map ~f:t_from_input expected in
        let actual =
          Source_opam_config.Private.Opam_repositories.from_opam_value value
        in
        Alcotest.(check (result opam_url_set Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Simple" ~value:{|[ "https://github.com/repo" ]|}
        ~expected:(Ok [ "https://github.com/repo" ]) ();
      make_test ~name:"Multiple" ~value:{|[ "https://a.com" "https://b.com" ]|}
        ~expected:(Ok [ "https://a.com"; "https://b.com" ])
        ();
    ]

  let test_to_opam_value =
    let make_test ~name ~url_set ~expected () =
      let test_name =
        Printf.sprintf "Opam_repositories.to_opam_value: %s" name
      in
      let test_fun () =
        let url_set = t_from_input url_set in
        let actual =
          Source_opam_config.Private.Opam_repositories.to_opam_value url_set
          |> OpamPrinter.FullPos.value
        in
        Alcotest.(check string) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Simple" ~url_set:[ "https://github.com" ]
        ~expected:{|["https://github.com"]|} ();
      make_test ~name:"Multiple"
        ~url_set:[ "https://a.com"; "https://b.com" ]
        ~expected:{|["https://a.com" "https://b.com"]|} ();
    ]

  let test_cmdliner_parse =
    let make_test ~name ~value ~expected =
      let test_name =
        Printf.sprintf "Opam_repositories.cmdliner_parse: %s" name
      in
      let test_fun () =
        let open Source_opam_config.Private in
        let expected = Result.map ~f:t_from_input expected in
        let actual =
          Cmdliner.Arg.conv_parser Opam_repositories.cmdliner_conv value
        in
        Alcotest.(check (result opam_url_set Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Simple" ~value:"[https://a.com]"
        ~expected:(Ok [ "https://a.com" ]);
      make_test ~name:"Simple" ~value:"[https://a.com,https://b.com]"
        ~expected:(Ok [ "https://a.com"; "https://b.com" ]);
    ]
end

module Opam_global_vars = struct
  let test_from_opam_value =
    let make_test ~name ~value ~expected () =
      let test_name =
        Printf.sprintf "Opam_global_vars.from_opam_value: %s" name
      in
      let test_fun () =
        let value = OpamParser.FullPos.value_from_string value "test.opam" in
        let expected = Result.map expected ~f:String.Map.of_list_exn in
        let actual =
          let open Duniverse_lib.Source_opam_config in
          Private.Opam_global_vars.from_opam_value value
        in
        Alcotest.(check (result variable_content_string_map Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Simple"
        ~value:{|[["var1" true] ["var2" "a"] ["var3" ["a" "b"]]]|}
        ~expected:
          (Ok [ ("var1", B true); ("var2", S "a"); ("var3", L [ "a"; "b" ]) ])
        ();
      make_test ~name:"Collision" ~value:{|[["var1" true]["var1" true]]|}
        ~expected:
          (Error
             (`Msg
               "Error in opam file test.opam, [1:0]-[1:28]: Opam global \
                variable var1 is defined more than once"))
        ();
    ]

  let test_to_opam_value =
    let make_test ~name ~env ~expected () =
      let test_name =
        Printf.sprintf "Opam_global_vars.to_opam_value: %s" name
      in
      let test_fun () =
        let env = String.Map.of_list_exn env in
        let actual =
          let open Duniverse_lib.Source_opam_config in
          Private.Opam_global_vars.to_opam_value env
          |> OpamPrinter.FullPos.value
        in
        Alcotest.(check string) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Simple"
        ~env:[ ("var1", B true); ("var2", S "a"); ("var3", L [ "a"; "b" ]) ]
        ~expected:{|[["var1" true] ["var2" "a"] ["var3" ["a" "b"]]]|} ();
    ]

  let test_cmdliner_parse =
    let make_test ~name ~value ~expected =
      let test_name =
        Printf.sprintf "Opam_global_vars.cmdliner_parse: %s" name
      in
      let test_fun () =
        let open Source_opam_config.Private in
        let expected = Result.map expected ~f:String.Map.of_list_exn in
        let actual =
          Cmdliner.Arg.conv_parser Opam_global_vars.cmdliner_conv value
        in
        Alcotest.(check (result variable_content_string_map Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Simple" ~value:"[[var1,true]]"
        ~expected:(Ok [ ("var1", B true) ]);
      make_test ~name:"Multiple" ~value:"[[var1,true],[var2,a],[var3,[a,b]]]"
        ~expected:
          (Ok [ ("var1", B true); ("var2", S "a"); ("var3", L [ "a"; "b" ]) ]);
    ]
end

module Opam_provided = struct
  let t_from_input l =
    List.map ~f:OpamPackage.Name.of_string l |> OpamPackage.Name.Set.of_list

  let test_from_opam_value =
    let make_test ~name ~value ~expected =
      let test_name = Printf.sprintf "Opam_provided.from_opam_value: %s" name in
      let test_fun () =
        let open Source_opam_config in
        let value = OpamParser.FullPos.value_from_string value "test.opam" in
        let expected = Result.map expected ~f:t_from_input in
        let actual = Private.Opam_provided.from_opam_value value in
        Alcotest.(check (result Testable.opam_package_name_set Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Single" ~value:{|"a"|} ~expected:(Ok [ "a" ]);
      make_test ~name:"List" ~value:{|["a" "b"]|} ~expected:(Ok [ "a"; "b" ]);
      make_test ~name:"Error" ~value:{|42|}
        ~expected:
          (Rresult.R.error_msg
             "Error in opam file test.opam, [1:0]-[1:2]: Expected a list or a \
              string, got: 42");
    ]

  let test_to_opam_value =
    let make_test ~name ~value ~expected =
      let test_name = Printf.sprintf "Opam_provided.to_opam_value: %s" name in
      let test_fun () =
        let open Source_opam_config in
        let value = t_from_input value in
        let actual =
          Private.Opam_provided.to_opam_value value |> OpamPrinter.FullPos.value
        in
        Alcotest.(check string) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [ make_test ~name:"Simple" ~value:[ "a"; "b" ] ~expected:{|["a" "b"]|} ]
end

let suite =
  ( "Source_opam_config",
    List.concat
      [
        Opam_repositories_url_rewriter.test_rewrite_one_in;
        Opam_repositories_url_rewriter.test_rewrite_one_out;
        Opam_repositories.test_from_opam_value;
        Opam_repositories.test_to_opam_value;
        Opam_repositories.test_cmdliner_parse;
        Opam_global_vars.test_from_opam_value;
        Opam_global_vars.test_to_opam_value;
        Opam_global_vars.test_cmdliner_parse;
        Opam_provided.test_from_opam_value;
        Opam_provided.test_to_opam_value;
      ] )
