open Cmdliner

let non_deterministic =
  let doc = "Run non-deterministic tests." in
  Arg.(value & flag & info ["non-deterministic"; "n"] ~doc)

let file =
  let doc = "The file to use." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"FILE")

let section =
  let doc =
    "Select file sub-sections. Will be interpreted as a Perl regular expression."
  in
  Arg.(value & opt (some string) None & info ["section"; "s"] ~doc ~docv:"PAT")

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())
