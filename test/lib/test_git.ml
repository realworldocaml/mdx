module Testable = struct
  include Testable

  let commit_pointed_by_error =
    let equal err err' =
      match (err, err') with
      | `No_such_ref, `No_such_ref -> true
      | `Multiple_such_refs, `Multiple_such_refs -> true
      | `Msg s, `Msg s' -> String.equal s s'
      | _ -> false
    in
    let pp fmt = function
      | `Msg _ as m -> Rresult.R.pp_msg fmt m
      | `No_such_ref -> Format.pp_print_string fmt "`No_such_ref"
      | `Multiple_such_refs -> Format.pp_print_string fmt "Multiple_such_refs"
    in
    Alcotest.testable pp equal

  let branch_of_symref_error =
    let equal err err' =
      match (err, err') with
      | `Not_a_symref, `Not_a_symref -> true
      | `Msg s, `Msg s' -> String.equal s s'
      | _ -> false
    in
    let pp fmt = function
      | `Msg _ as m -> Rresult.R.pp_msg fmt m
      | `Not_a_symref -> Format.pp_print_string fmt "`Not_a_symref"
    in
    Alcotest.testable pp equal
end

module Ls_remote = struct
  let test_parse_output_line =
    let make_test ~name ~line ~expected () =
      let test_name = Printf.sprintf "Ls_remote.parse_output_line: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Git.Ls_remote.parse_output_line line in
        Alcotest.(check (result (pair string string) Testable.r_msg))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Ok" ~line:"12ab  refs/tags/v1"
        ~expected:(Ok ("12ab", "refs/tags/v1"))
        ();
      make_test ~name:"Error" ~line:"12ab  refs/tags/v1 something"
        ~expected:
          (Error
             (`Msg
               "Invalid git ls-remote output line: \"12ab  refs/tags/v1 \
                something\""))
        ();
    ]

  let test_commit_pointed_by =
    let make_test ~name ~ref ~lines ~expected () =
      let test_name = Printf.sprintf "Ls_remote.commit_pointed_by: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Git.Ls_remote.commit_pointed_by ~ref lines in
        Alcotest.(check (result string Testable.commit_pointed_by_error))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Empty output" ~ref:"v1" ~lines:[]
        ~expected:(Error `No_such_ref) ();
      make_test ~name:"Not in output" ~ref:"v1"
        ~lines:[ "0001    refs/heads/master" ]
        ~expected:(Error `No_such_ref) ();
      make_test ~name:"Invalid output" ~ref:"v1" ~lines:[ "invalid-output" ]
        ~expected:
          (Error (`Msg "Invalid git ls-remote output line: \"invalid-output\""))
        ();
      make_test ~name:"Regular repo" ~ref:"v1"
        ~lines:[ "0001   refs/heads/master"; "0002   refs/tags/v1" ]
        ~expected:(Ok "0002") ();
      make_test ~name:"Repo with packed refs" ~ref:"v1"
        ~lines:
          [
            "0001   refs/heads/master";
            "0002   refs/tags/v1";
            "0003   refs/tags/v1^{}";
          ]
        ~expected:(Ok "0003") ();
      make_test ~name:"Order doesn't matter" ~ref:"v1"
        ~lines:
          [
            "0003   refs/tags/v1^{}";
            "0001   refs/heads/master";
            "0002   refs/tags/v1";
          ]
        ~expected:(Ok "0003") ();
      make_test ~name:"Works with branches" ~ref:"some-branch"
        ~lines:[ "0001    refs/heads/master"; "0002   refs/heads/some-branch" ]
        ~expected:(Ok "0002") ();
      make_test ~name:"Points to several commits" ~ref:"abc"
        ~lines:[ "001   refs/heads/abc"; "002   refs/tags/abc" ]
        ~expected:(Error `Multiple_such_refs) ();
      make_test ~name:"Points to several commits (with packed-refs)" ~ref:"abc"
        ~lines:
          [
            "001   refs/heads/abc";
            "002   refs/heads/abc^{}";
            "003   refs/tags/abc";
            "004   refs/tags/abc^{}";
          ]
        ~expected:(Error `Multiple_such_refs) ();
      make_test ~name:"Empty output" ~ref:"abc" ~lines:[ "" ]
        ~expected:(Error `No_such_ref) ();
      make_test ~name:"Not branch or tag" ~ref:"abc"
        ~lines:
          [
            "001   refs/heads/master";
            "002   refs/import/tags/abc";
            "003   refs/tags/abc";
          ]
        ~expected:(Ok "003") ();
      make_test ~name:"Same suffix" ~ref:"abc"
        ~lines:[ "001   refs/heads/xabc" ]
        ~expected:(Error `No_such_ref) ();
      make_test ~name:"Ref is a commit" ~ref:"000456"
        ~lines:[ "00017f   refs/heads/master"; "000456   refs/heads/abc" ]
        ~expected:(Ok "000456") ();
      make_test ~name:"Ref looks like a commit"
        ~ref:"7af9de1c4c468d8fd0f2870b98355803bcfd76f7"
        ~lines:[ "000000   refs/heads/master" ]
        ~expected:(Ok "7af9de1c4c468d8fd0f2870b98355803bcfd76f7") ();
    ]

  let test_branch_of_symref =
    let make_test ~name ~symref ~lines ~expected () =
      let test_name = Printf.sprintf "Ls_remote.branch_of_symref: %s" name in
      let test_fun () =
        let actual =
          Duniverse_lib.Git.Ls_remote.branch_of_symref ~symref lines
        in
        Alcotest.(check (result string Testable.branch_of_symref_error))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [
      make_test ~name:"Empty output" ~symref:"abc" ~lines:[ "" ]
        ~expected:(Error `Not_a_symref) ();
      make_test ~name:"No output" ~symref:"abc" ~lines:[]
        ~expected:(Error `Not_a_symref) ();
      make_test ~name:"Typical output" ~symref:"HEAD"
        ~lines:[ "ref: refs/heads/master  HEAD"; "0000        HEAD" ]
        ~expected:(Ok "master") ();
      make_test ~name:"Another typical output" ~symref:"HEAD"
        ~lines:[ "ref: refs/heads/main  HEAD"; "0001        HEAD" ]
        ~expected:(Ok "main") ();
      make_test ~name:"Local project" ~symref:"HEAD"
        ~lines:
          [
            "ref: refs/heads/mirage-4        HEAD";
            "0002        HEAD";
            "ref: refs/remotes/origin/master refs/remotes/origin/HEAD";
            "0003        refs/remotes/origin/HEAD";
          ]
        ~expected:(Ok "mirage-4") ();
      make_test ~name:"Error when HEAD points towards multiple branches"
        ~symref:"HEAD"
        ~lines:
          [
            "ref: refs/heads/master        HEAD";
            "ref: refs/heads/main          HEAD";
            "ref: refs/heads/trunk         HEAD";
            "0004     HEAD";
          ]
        ~expected:
          (Error
             (`Msg
               "Invalid `git ls-remote --symref` output. Too many lines \
                starting by `ref:`."))
        ();
      make_test ~name:"Error when symref doesn't point to a branch"
        ~symref:"symref"
        ~lines:[ "ref: refs/tags/v0.1        symref"; "0005     symref" ]
        ~expected:
          (Error
             (`Msg
               "Invalid `git ls-remote --symref` output. Failed to extract \
                branch from ref `refs/tags/v0.1`."))
        ();
    ]
end

let suite =
  ( "Git",
    Ls_remote.test_parse_output_line @ Ls_remote.test_commit_pointed_by
    @ Ls_remote.test_branch_of_symref )
