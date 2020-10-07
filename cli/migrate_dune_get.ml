open Duniverse_lib
open Rresult

let run (`Repo repo) () =
  let dune_get = Fpath.(repo // Config.dune_get) in
  let duniverse_res = (Duniverse.load_dune_get ~file:dune_get [@warning "-3"]) in
  duniverse_res >>= fun duniverse ->
  Repo.duniverse_file repo >>= fun file -> Duniverse.save ~file duniverse

let info =
  let open Cmdliner in
  let doc = "convert a legacy dune-get file to an opam lock file" in
  let exits = Term.default_exits in
  Term.info "migrate-dune-get" ~doc ~exits

let term = Cmdliner.Term.(term_result (const run $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
