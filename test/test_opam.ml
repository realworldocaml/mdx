module Testable = struct
  module Dev_repo = struct
    open Duniverse_lib.Opam.Dev_repo

    let t = Alcotest.testable pp equal
  end
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
    [ make_test ~dev_repo:"https://host.com/repo"
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
        ()
    ]
end

let suite = ("Opam", Dev_repo.test_from_string)
