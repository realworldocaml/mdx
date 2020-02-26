let read_opam v =
  let open Sexplib in
  let t = OpamFile.OPAM.read_from_string v in
  let depexts = OpamFile.OPAM.depexts t in
  let sexp = Sexp.List (
    List.map
    (fun (depexts, filter) ->
      let d = Sexp.List (List.map (fun x -> Sexp.Atom x) depexts) in
      let f = Sexp.Atom (OpamFilter.to_string filter) in
      Sexp.List [ d; f ])
    depexts) in
    print_endline (Sexp.to_string_hum sexp);
    ()

let run (`Opam_file file) () =
  let open Rresult.R in
  Bos.OS.File.read (Fpath.v file) >>= fun v ->
  read_opam v;
  Ok ()

let opam_file =
  let open Cmdliner in
  let doc = "TODO" in
  Common.Arg.named
    (fun x -> `Opam_file x)
    Arg.(value & opt string "opam" & info [ "f" ] ~docv:"FILE" ~doc)

let info =
  let open Cmdliner in
  let doc = "show depexts" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P "TODO";
    ]
  in
  Term.info "depext" ~doc ~exits ~man 

let term =
  Cmdliner.Term.(term_result (const run $ opam_file $ Common.Arg.setup_logs ()))

let cmd = (term, info)


