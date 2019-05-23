let run repo () = Duniverse_lib.Dune_cmd.gen_dune_lock repo

let info =
  let open Cmdliner in
  let doc = "generate git tags suitable for vendoring from opam metadata" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description;
      `P
        "This command takes the standalone opam package list calculated using $(i,duniverse opam) \
         and converts it into a set of git tags that can be put into the $(b,duniverse/) \
         subdirectory and act as vendored copies of the source code.";
      `P
        "The opam packages are sorted by their Git remotes and deduplicated so that only one \
         clone is used per Git remote.  In the event of multiple conflicting tags being found, \
         the latest one is selected and a warning printed.  The output is stored in \
         $(b,.duniverse/dune.sxp) and can be hand-edited if necessary to tweak your vendoring \
         setup.";
      `P
        "This is currently a separate command as it will be extended in the future to non-opam \
         package managers as well."
    ]
  in
  Term.info "lock" ~doc ~exits ~man

let term = Cmdliner.Term.(term_result (const run $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
