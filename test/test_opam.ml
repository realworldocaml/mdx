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
        let url = OpamFile.URL.create (OpamUrl.parse url_src) in
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

let suite = ("Opam", List.concat [ Url.test_from_opam ])
