open Duniverse_lib
open Duniverse_lib.Types

let compute_opam ~repo ~branch ~explicit_root_packages ~excludes ~pins ~remotes =
  let open Rresult.R.Infix in
  Opam_cmd.find_local_opam_packages repo >>= fun local_packages ->
  Opam_cmd.choose_root_packages ~explicit_root_packages ~local_packages >>= fun root_packages ->
  let root_packages =
    List.map Opam_cmd.split_opam_name_and_version root_packages |> Opam.sort_uniq
  in
  let excludes =
    List.map Opam_cmd.split_opam_name_and_version (local_packages @ excludes) |> Opam.sort_uniq
  in
  Bos.OS.Dir.tmp ".duniverse-opam-root-%s" >>= fun root ->
  Opam_cmd.init_opam ~root ~remotes () >>= fun () ->
  Exec.(iter (add_opam_dev_pin ~root) pins) >>= fun () ->
  Exec.(iter (add_opam_local_pin ~root) local_packages) >>= fun () ->
  Opam_cmd.calculate_opam ~root ~root_packages ~pins ~excludes ~remotes ~branch >>= fun opam ->
  let packages_stats = Opam_cmd.packages_stats opam.packages in
  Opam_cmd.report_packages_stats packages_stats;
  Ok opam

let compute_repos ~packages =
  let open Rresult.R.Infix in
  let packages = Dune_cmd.filter_invalid_packages packages in
  let dune_packages = List.filter (fun o -> o.Opam.is_dune) packages in
  Exec.map Dune_cmd.dune_repo_of_opam dune_packages >>= fun repos ->
  Dune_cmd.dedup_git_remotes repos

let run repo branch explicit_root_packages excludes pins remotes () =
  let open Rresult.R.Infix in
  Common.Logs.app (fun l -> l "Calculating Duniverse on the %a branch" Styled_pp.branch branch);
  compute_opam ~repo ~branch ~explicit_root_packages ~excludes ~pins ~remotes >>= fun opam ->
  let { Opam.root_packages; excludes; pins; remotes; branch; packages } = opam in
  Common.Logs.app (fun l -> l "Calculating Git repositories to vendor");
  compute_repos ~packages >>= fun repos ->
  let duniverse = { Duniverse.root_packages; excludes; pins; remotes; branch; packages; repos } in
  let file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.save ~file duniverse >>= fun () ->
  Common.Logs.app (fun l ->
      l "Wrote duniverse file with %a entries to %a."
        Fmt.(styled `Green int)
        (List.length repos)
        Fmt.(styled `Cyan Fpath.pp)
        (Fpath.normalize file) );
  Ok ()

open Cmdliner

let branch =
  let doc =
    "Branch that represents the working tree of the source code. Defaults to $(i,master)"
  in
  Cmdliner.Arg.(value & opt string "master" & info [ "b" ] ~docv:"BRANCH" ~doc)

let explicit_root_packages =
  let doc =
    "opam packages to calculate duniverse for. If not supplied, any local opam metadata files are \
     used as the default package list."
  in
  Arg.(value & pos_all string [] & info [] ~doc ~docv:"PACKAGES")

let excludes =
  let doc =
    "Packages to exclude from the output list. You can use this to remove the root packages so \
     they are not duplicated in the vendor directory.  Repeat this flag multiple times for more \
     than one exclusion."
  in
  Arg.(value & opt_all string [] & info [ "exclude"; "x" ] ~docv:"EXCLUDE" ~doc)

let pins =
  let open Types.Opam in
  let doc =
    "Packages to pin for the latest opam metadata and source. You can separate the package name \
     and a url and a remote branch via commas to specify a manual url (e.g. \
     $(i,mirage,git://github.com/avsm/mirage,fixme)).  If a url is not specified then the \
     $(i,--dev) pin is used.  If a branch is not specified then the default remote branch is \
     used. Repeat this flag multiple times for more than one exclusion."
  in
  let fin s =
    match Astring.String.cuts ~sep:"," s with
    | [] -> failwith "unexpected pin error"
    | [ pin ] -> Ok { pin; url = None; tag = None }
    | [ pin; url ] -> Ok { pin; url = Some url; tag = None }
    | [ pin; url; tag ] -> Ok { pin; url = Some url; tag = Some tag }
    | _ -> failwith "pins must have maximum of 3 commas"
  in
  let fout ppf { pin; url; tag } =
    match (url, tag) with
    | None, _ -> Fmt.(pf ppf "%s" pin)
    | Some url, None -> Fmt.(pf ppf "%s,%s" pin url)
    | Some url, Some tag -> Fmt.(pf ppf "%s,%s,%s" pin url tag)
  in
  let t = Arg.conv ~docv:"PIN" (fin, fout) in
  Arg.(value & opt_all t [] & info [ "pin"; "p" ] ~docv:"PIN" ~doc)

let remotes =
  let doc =
    "Extra opam remotes to add when resolving package names. Repeat this flag multiple times for \
     more than one remote."
  in
  Arg.(value & opt_all string [] & info [ "opam-remote" ] ~docv:"OPAM_REMOTE" ~doc)

let info =
  let exits = Term.default_exits in
  let doc = "analyse opam files to generate an initial $(b,duniverse.sxp)" in
  let man = [] in
  Term.info "init" ~doc ~exits ~man

let term =
  let open Term in
  term_result
    ( const run $ Common.Arg.repo $ branch $ explicit_root_packages $ excludes $ pins $ remotes
    $ Common.Arg.setup_logs () )

let cmd = (term, info)
