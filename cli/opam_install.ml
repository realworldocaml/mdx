let run (`Yes yes) (`Repo repo) () = Duniverse_lib.Opam_cmd.install_incompatible_packages yes repo

let info =
  let open Cmdliner in
  let doc = "install packages that are not duniverse-compatible via opam" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command generates and execute an opam command that will install \
         duniverse-incompatible packages in the global opam switch. By default it prompts for \
         confirmation.";
    ]
  in
  Term.info "opam-install" ~doc ~exits ~man

let term =
  Cmdliner.Term.(
    term_result (const run $ Common.Arg.yes $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
