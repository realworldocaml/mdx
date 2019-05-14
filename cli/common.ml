module Arg = struct
  let fpath = Cmdliner.Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

  let repo =
    let doc = "Path to Git repository to store vendored code in." in
    Cmdliner.Arg.(
      value & opt fpath (Fpath.v ".") & info [ "r"; "--repo" ] ~docv:"TARGET_REPO" ~doc)

  let branch =
    let doc =
      "Branch that represents the working tree of the source code. Defaults to $(i,master)"
    in
    Cmdliner.Arg.(value & opt string "master" & info [ "b" ] ~docv:"BRANCH" ~doc)

  let setup_logs () =
    Printexc.record_backtrace true;
    let setup_log style_renderer level =
      Fmt_tty.setup_std_outputs ?style_renderer ();
      Logs.set_level level;
      Logs.set_reporter (Logs_fmt.reporter ())
    in
    let global_option_section = "COMMON OPTIONS" in
    let open Cmdliner.Term in
    const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ()
end
