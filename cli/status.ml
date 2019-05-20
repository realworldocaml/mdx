let run repo branch () = Duniverse_lib.Dune_cmd.status repo branch

let info =
  let open Cmdliner in
  let doc = "summarise the libraries tracked by the duniverse" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description;
      `P
        "This command looks at the various metadata files in the $(b,.duniverse) directory and \
         prints them out in a human-readable format."
    ]
  in
  Term.info "status" ~doc ~exits ~man

let term =
  Cmdliner.Term.(
    term_result (const run $ Common.Arg.repo $ Common.Arg.branch $ Common.Arg.setup_logs ()))

let cmd = (term, info)
