module Testable = struct
  include Testable

  let commit_pointed_by_error =
    let equal err err' =
      match (err, err') with
      | `No_such_ref, `No_such_ref -> true
      | `Msg s, `Msg s' -> String.equal s s'
      | _ -> false
    in
    let pp fmt = function
      | `Msg _ as m -> Rresult.R.pp_msg fmt m
      | `No_such_ref -> Format.pp_print_string fmt "`No_such_ref"
    in
    Alcotest.testable pp equal
end

module Ls_remote = struct
  let test_parse_output_line =
    let make_test ~name ~line ~expected () =
      let test_name = Printf.sprintf "Ls_remote.parse_output_line: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Git.Ls_remote.parse_output_line line in
        Alcotest.(check (result (pair string string) Testable.r_msg)) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [ make_test ~name:"Ok" ~line:"12ab  refs/tags/v1" ~expected:(Ok ("12ab", "refs/tags/v1")) ();
      make_test ~name:"Error" ~line:"12ab  refs/tags/v1 something"
        ~expected:
          (Error (`Msg "Invalid git ls-remote output line: \"12ab  refs/tags/v1 something\""))
        ()
    ]

  let test_commit_pointed_by =
    let make_test ~name ~ref ~lines ~expected () =
      let test_name = Printf.sprintf "Ls_remote.commit_pointed_by: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Git.Ls_remote.commit_pointed_by ~ref lines in
        Alcotest.(check (result string Testable.commit_pointed_by_error)) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [ make_test ~name:"Empty output" ~ref:"v1" ~lines:[] ~expected:(Error `No_such_ref) ();
      make_test ~name:"Not in output" ~ref:"v1" ~lines:[ "0001    refs/heads/master" ]
        ~expected:(Error `No_such_ref)
        ();
      make_test ~name:"Invalid output" ~ref:"v1" ~lines:[ "invalid-output" ]
        ~expected:(Error (`Msg "Invalid git ls-remote output line: \"invalid-output\""))
        ();
      make_test ~name:"Regular repo" ~ref:"v1"
        ~lines:[ "0001   refs/heads/master"; "0002   refs/tags/v1" ]
        ~expected:(Ok "0002") ();
      make_test ~name:"Repo with packed refs" ~ref:"v1"
        ~lines:[ "0001   refs/heads/master"; "0002   refs/tags/v1"; "0003   refs/tags/v1^{}" ]
        ~expected:(Ok "0003") ();
      make_test ~name:"Order doesn't matter" ~ref:"v1"
        ~lines:[ "0003   refs/tags/v1^{}"; "0001   refs/heads/master"; "0002   refs/tags/v1" ]
        ~expected:(Ok "0003") ();
      make_test ~name:"Works with branches" ~ref:"some-branch"
        ~lines:[ "0001    refs/heads/master"; "0002   refs/heads/some-branch" ]
        ~expected:(Ok "0002") ()
    ]
end

let suite = ("Git", Ls_remote.test_parse_output_line @ Ls_remote.test_commit_pointed_by)
