module Arg = struct
  let named f = Cmdliner.Term.(app (const f))

  let fpath = Cmdliner.Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

  let repo =
    let doc = "Path to Git repository to store vendored code in." in
    named
      (fun x -> `Repo x)
      Cmdliner.Arg.(
        value & opt fpath (Fpath.v ".") & info [ "r"; "repo" ] ~docv:"TARGET_REPO" ~doc)

  let yes =
    let doc = "Do not prompt for confirmation and always assume yes" in
    named (fun x -> `Yes x) Cmdliner.Arg.(value & flag & info [ "y"; "yes" ] ~doc)

  let thread_safe_reporter reporter =
    let lock = Mutex.create () in
    let { Logs.report } = reporter in
    let oui src level ~over k msgf =
      Mutex.lock lock;
      let x = report src level ~over k msgf in
      Mutex.unlock lock;
      x
    in
    Logs.{ report = oui }

  let setup_logs () =
    Printexc.record_backtrace true;
    let setup_log style_renderer level =
      Fmt_tty.setup_std_outputs ?style_renderer ();
      Logs.set_level level;
      Logs.set_reporter (thread_safe_reporter (Logs_fmt.reporter ()))
    in
    let global_option_section = "COMMON OPTIONS" in
    let open Cmdliner.Term in
    const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ()
end

module Logs = struct
  let app ?src f =
    Logs.app ?src (fun l ->
        f (fun ?header ?tags fmt -> l ?header ?tags ("%a" ^^ fmt) Styled_pp.header ()) )
end
