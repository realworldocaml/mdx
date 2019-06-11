module Testable = struct
  module Element = struct
    open Duniverse_lib.Duniverse.Element

    let t = Alcotest.testable pp equal
  end

  module R_msg = struct
    let t = Alcotest.testable Rresult.R.pp_msg (fun (`Msg s) (`Msg s') -> String.equal s s')
  end
end

module Element = struct
  let test_from_opam_entry =
    let open Duniverse_lib.Types.Opam in
    let open Duniverse_lib.Duniverse.Element in
    let entry_factory ?(package = { name = ""; version = None }) ?(dev_repo = `Virtual) ?tag
        ?(is_dune = false) () =
      { package; dev_repo; tag; is_dune }
    in
    let make_test ?(get_default_branch = fun _ -> assert false) ~name ~entry ~expected () =
      let test_name = Printf.sprintf "Element.from_opam_entry: %s" name in
      let test_fun () =
        let actual = from_opam_entry ~get_default_branch entry in
        Alcotest.(check (result (option Testable.Element.t) Testable.R_msg.t))
          test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [ make_test ~name:"Virtual" ~entry:(entry_factory ~dev_repo:`Virtual ()) ~expected:(Ok None) ();
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
          (entry_factory ~dev_repo:(`Git "x") ~is_dune:true ~package:{ name = "y"; version = None }
             ~tag:"z" ())
        ~expected:(Ok (Some (Repo { dir = "y"; upstream = "x"; ref = "z" })))
        ()
    ]

  let test_dedup_upstream =
    let make_test ~name ~l ~expected () =
      let test_name = Printf.sprintf "Element.dedup_upstream: %s" name in
      let test_fun () =
        let actual = Duniverse_lib.Duniverse.Element.dedup_upstream l in
        Alcotest.(check (list Testable.Element.t)) test_name expected actual
      in
      (test_name, `Quick, test_fun)
    in
    [ make_test ~name:"Empty" ~l:[] ~expected:[] ();
      make_test ~name:"Do not dedup opams"
        ~l:[ Opam { name = "x"; version = None }; Opam { name = "x"; version = None } ]
        ~expected:[ Opam { name = "x"; version = None }; Opam { name = "x"; version = None } ]
        ();
      make_test ~name:"Dedup upstreams and pick shortest name as dir"
        ~l:
          [ Repo { dir = "a"; upstream = "u"; ref = "x" };
            Repo { dir = "a-lwt"; upstream = "u"; ref = "x" }
          ]
        ~expected:[ Repo { dir = "a"; upstream = "u"; ref = "x" } ]
        ();
      make_test ~name:"Dedup upstreams and pick latest ref"
        ~l:
          [ Repo { dir = "a"; upstream = "u"; ref = "v1.9.0" };
            Repo { dir = "a-lwt"; upstream = "u"; ref = "v1.10.0" }
          ]
        ~expected:[ Repo { dir = "a"; upstream = "u"; ref = "v1.10.0" } ]
        ()
    ]
end

let suite = ("Duniverse", Element.test_from_opam_entry @ Element.test_dedup_upstream)
