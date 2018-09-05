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
open Astring

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

let target_repo_t =
  let doc = "Path to Git repository to store vendored code in." in
  let open Arg in
  value
  & opt fpath_t (Fpath.v ".")
  & info ["r"; "--repo"] ~docv:"TARGET_REPO" ~doc

let branch_t =
  let doc =
    "Branch that represents the working tree of the source code.
     If not supplied, the $(i,master) branch is used."
  in
  Arg.(value & opt string "master" & info ["b"] ~docv:"VENDOR_BRANCH" ~doc)

let exclude_t =
  let doc =
    "Packages to exclude from the output list. You can use this to remove the \
     root packages so they are not duplicated in the vendor directory.  \
     Repeat this flag multiple times for more than one exclusion."
  in
  Arg.(value & opt_all string [] & info ["exclude"; "x"] ~docv:"EXCLUDE" ~doc)

let remotes_t =
  let doc =
    "Extra opam remotes to add when resolving package names. \
     Repeat this flag multiple times for more than one remote."
  in
  Arg.(value & opt_all string [] & info ["opam-remote"] ~docv:"OPAM_REMOTE" ~doc)

let pins_t =
  let open Types.Opam in
  let doc =
    "Packages to pin for the latest opam metadata and source. You can separate the package name and a url and a remote branch via commas to specify a manual url (e.g. $(i,mirage,git://github.com/avsm/mirage,fixme)).  If a url is not specified then the $(i,--dev) pin is used.  If a branch is not specified then $(i,master) is used. Repeat this flag multiple times for more than one exclusion."
  in
  let fin s =
    match String.cuts ~sep:"," s with
    | [] -> failwith "unexpected pin error"
    | [pin] -> Ok {pin;url=None;tag=None}
    | [pin;url] -> Ok {pin;url=Some url;tag=None}
    | [pin;url;tag] -> Ok {pin; url=Some url; tag=Some tag}
    | _ -> failwith "pins must have maximum of 3 commas" in
  let fout ppf {pin;url;tag} =
    match url,tag with
    | None,_ -> Fmt.(pf ppf "%s" pin)
    | Some url, None -> Fmt.(pf ppf "%s,%s" pin url)
    | Some url, Some tag -> Fmt.(pf ppf "%s,%s,%s" pin url tag) in
  let t = Arg.conv ~docv:"PIN" (fin,fout) in
  Arg.(value & opt_all t [] & info ["pin"; "p"] ~docv:"PIN" ~doc)

let ocaml_switch_t =
  let doc =
    "Name of the ocaml compiler to use to resolve opam packages.  A local \
     switch is created in $(i,.duniverse) where pins and packages can be \
     tracked independently of your main opam switch.  This defaults to \
     $(i,ocaml-system), but you can use this flag to supply a more specific \
     version such as $(b,ocaml.4.06.1)."
  in
  let open Arg in
  value & opt string "ocaml-system"
  & info ["opam-switch"; "s"] ~docv:"OPAM_SWITCH" ~doc

let pkg_t =
  let open Arg in
  value & pos_all string []
  & info [] ~doc:"opam packages to calculate duniverse for" ~docv:"PACKAGES"

let opam_cmd =
  let doc = "opam TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result
      ( const Opam_cmd.init_duniverse
      $ target_repo_t $ branch_t $ pkg_t $ exclude_t $ pins_t $ ocaml_switch_t $ remotes_t 
      $ setup_logs () ))
  , Term.info "opam" ~doc ~exits ~man )

let dune_lock_cmd =
  let doc = "dune-lock TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result (const Dune_cmd.gen_dune_lock $ target_repo_t $ setup_logs ()))
  , Term.info "lock" ~doc ~exits ~man )

let dune_lockfile_t =
  let doc = "Input path of Dune lockfile" in
  let open Arg in
  value
  & opt fpath_t Config.duniverse_lockfile
  & info ["f"] ~docv:"DUNE_LOCKFILE" ~doc

let dune_fetch_cmd =
  let doc = "dune-fetch TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result
      ( const Dune_cmd.gen_dune_upstream_branches
      $ target_repo_t $ setup_logs () ))
  , Term.info "pull" ~doc ~exits ~man )

let vendor_lock_cmd =
  let doc = "vendor-lock TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result
      ( const Git_cmd.update $ target_repo_t $ branch_t $ pkg_t $ exclude_t
      $ pins_t $ ocaml_switch_t $ remotes_t $ setup_logs () ))
  , Term.info "vendor-lock" ~doc ~exits ~man )

let vendor_pull_cmd =
  let doc = "vendor-pull TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result (const Git_cmd.pull $ target_repo_t $ branch_t $ setup_logs ()))
  , Term.info "vendor-pull" ~doc ~exits ~man )

let vendor_merge_cmd =
  let doc = "vendor-merge TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result (const Git_cmd.merge $ target_repo_t $ branch_t $ setup_logs ()))
  , Term.info "vendor-merge" ~doc ~exits ~man )

let status_cmd =
  let doc = "status TODO" in
  let exits = Term.default_exits in
  let man = [`S Manpage.s_description; `P "TODO"] in
  ( (let open Term in
    term_result
      (const Dune_cmd.status $ target_repo_t $ branch_t $ setup_logs ()))
  , Term.info "status" ~doc ~exits ~man )

let default_cmd =
  let doc = "duniverse is the spice of build life" in
  let sdocs = Manpage.s_common_options in
  let man = [`S Manpage.s_description] in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
  , Term.info "duniverse" ~version:"%%VERSION%%" ~doc ~sdocs ~man )

let cmds =
  [ opam_cmd
  ; dune_lock_cmd
  ; dune_fetch_cmd
  ; status_cmd
  ; vendor_lock_cmd
  ; vendor_pull_cmd
  ; vendor_merge_cmd ]

let () = Term.(exit @@ eval_choice default_cmd cmds)
