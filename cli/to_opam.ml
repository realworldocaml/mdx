open Duniverse_lib
open Rresult

let run (`Repo repo) () =
  let duniverse_file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.load ~file:duniverse_file >>= fun duniverse ->
  let opam = Duniverse.to_opam duniverse in
  let lock_file = Fpath.(repo // Config.duniverse_opam_file) in
  Bos.OS.File.with_oc lock_file (fun oc () ->
      OpamFile.OPAM.write_to_channel oc opam;
      close_out oc;
      Ok ())
    ()
  >>= fun res -> res

let info =
  let open Cmdliner in
  let doc = "convert a dune-get file to an opam lock file" in
  let exits = Term.default_exits in
  Term.info "to-opam" ~doc ~exits

let term =
  Cmdliner.Term.(term_result (const run $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
