(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Cmdliner

let copts = ()

let setup_logs () =
  Printexc.record_backtrace true ;
  let setup_log style_renderer level =
    Fmt_tty.setup_std_outputs ?style_renderer () ;
    Logs.set_level level ;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  let global_option_section = "COMMON OPTIONS" in
  let open Term in
  const setup_log
  $ Fmt_cli.style_renderer ~docs:global_option_section ()
  $ Logs_cli.level ~docs:global_option_section ()

let copts_t =
  let _docs = Manpage.s_common_options in
  Term.(const copts)

let fpath_t = Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

let opam_lock_cmd =
  let doc = "opam-lock TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  let pkg_t =
    let open Arg in
    required
    & pos 0 (some string) None
    & info [] ~doc:"opam package name to calculate duniverse for"
        ~docv:"PACKAGE"
  in
  let lockfile_t =
    let doc = "Output path to store opam lockfile to" in
    let open Arg in
    value
    & opt fpath_t (Fpath.v "duniverse-opam.lock")
    & info ["o"] ~docv:"OUTPUT_FILE" ~doc
  in
  ( (let open Term in
    term_result
      (const Opam_lock_cmd.init_duniverse $ pkg_t $ lockfile_t $ setup_logs ()))
  , Term.info "opam-lock" ~doc ~exits ~man )

let dune_lock_cmd =
  let doc = "dune-lock TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  let opam_lockfile_t =
    let doc = "Input path of opam lockfile " in
    let open Arg in
    value
    & opt fpath_t (Fpath.v "duniverse-opam.lock")
    & info ["i"] ~docv:"OPAM_LOCKFILE" ~doc
  in
  let dune_lockfile_t =
    let doc = "Output path to store Dune lockfile to" in
    let open Arg in
    value
    & opt fpath_t (Fpath.v "duniverse-dune.lock")
    & info ["o"] ~docv:"DUNE_LOCKFILE" ~doc
  in
  ( (let open Term in
    term_result
      ( const Dune_lock_cmd.gen_dune_lock
      $ opam_lockfile_t $ dune_lockfile_t $ setup_logs () ))
  , Term.info "dune-lock" ~doc ~exits ~man )

let default_cmd =
  let doc = "duniverse is the spice of build life" in
  let sdocs = Manpage.s_common_options in
  let man = [`S Manpage.s_description] in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "duniverse" ~version:"%%VERSION%%" ~doc ~sdocs ~man )

let cmds = [opam_lock_cmd; dune_lock_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
