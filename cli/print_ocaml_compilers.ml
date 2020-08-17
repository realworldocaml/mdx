open Stdune
open Duniverse_lib

let run (`Repo repo) () =
  let open Rresult.R in
  Repo.duniverse_file repo >>= fun duniverse_file ->
  Duniverse.load ~file:duniverse_file >>= fun config ->
  print_endline (String.concat ~sep:" " config.Duniverse.config.ocaml_compilers);
  Ok ()

let info =
  let open Cmdliner in
  let doc = "print OCaml compilers that are supported for this duniverse" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command prints the OCaml compilers that are supported by this\n\
        \ duniverse. The results can be piped to your continuous integration\n\
         system to install the compiler needed for a successful dune build.";
    ]
  in
  Term.info "print-ocaml-compilers" ~doc ~exits ~man

let term = Cmdliner.Term.(term_result (const run $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
