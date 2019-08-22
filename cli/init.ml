open Duniverse_lib
open Duniverse_lib.Types

let build_config ~local_packages ~branch ~explicit_root_packages ~excludes ~pins ~remotes ~overlay
    =
  let open Rresult.R.Infix in
  Opam_cmd.choose_root_packages ~explicit_root_packages ~local_packages >>= fun root_packages ->
  let root_packages =
    List.map Opam_cmd.split_opam_name_and_version root_packages |> Opam.sort_uniq
  in
  let excludes =
    List.map Opam_cmd.split_opam_name_and_version (local_packages @ excludes) |> Opam.sort_uniq
  in
  let remotes = overlay :: remotes in
  Ok { Duniverse.Config.root_packages; excludes; pins; remotes; branch }

let init_tmp_opam ~local_packages ~config:{ Duniverse.Config.remotes; pins; _ } =
  let open Rresult.R.Infix in
  Bos.OS.Dir.tmp ".duniverse-opam-root-%s" >>= fun root ->
  Opam_cmd.init_opam ~root ~remotes () >>= fun () ->
  Exec.(iter (add_opam_dev_pin ~root) pins) >>= fun () ->
  Exec.(iter (add_opam_local_pin ~root ~kind:"path") local_packages) >>= fun () -> Ok root

let report_stats ~packages =
  let packages_stats = Opam_cmd.packages_stats packages in
  Opam_cmd.report_packages_stats packages_stats

let compute_deps ~opam_entries =
  Dune_cmd.log_invalid_packages opam_entries;
  let get_default_branch remote = Exec.git_default_branch ~remote () in
  Duniverse.Deps.from_opam_entries ~get_default_branch opam_entries

let resolve_ref deps =
  let resolve_ref ~upstream ~ref = Exec.git_resolve ~remote:upstream ~ref in
  Duniverse.Deps.resolve ~resolve_ref deps

let run repo branch explicit_root_packages excludes pins overlay remotes () =
  let open Rresult.R.Infix in
  Common.Logs.app (fun l -> l "Calculating Duniverse on the %a branch" Styled_pp.branch branch);
  Opam_cmd.find_local_opam_packages repo >>= fun local_packages ->
  build_config ~local_packages ~branch ~explicit_root_packages ~excludes ~pins ~remotes ~overlay
  >>= fun config ->
  Common.Logs.app (fun l -> l "Initializing temporary opam switch");
  init_tmp_opam ~local_packages ~config >>= fun root ->
  Common.Logs.app (fun l ->
      l "Resolving opam dependencies for %a"
        Fmt.(list ~sep:(unit " ") Styled_pp.package)
        config.root_packages );
  Opam_cmd.calculate_opam ~root ~config >>= fun packages ->
  report_stats ~packages;
  Common.Logs.app (fun l -> l "Calculating Git repositories to vendor");
  compute_deps ~opam_entries:packages >>= fun unresolved_deps ->
  resolve_ref unresolved_deps >>= fun deps ->
  let duniverse = { Duniverse.config; deps } in
  let file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.save ~file duniverse >>= fun () ->
  Common.Logs.app (fun l ->
      l "Wrote duniverse file with %a entries to %a."
        Fmt.(styled `Green int)
        (Duniverse.Deps.count duniverse.deps)
        Styled_pp.path (Fpath.normalize file) );
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

let overlay =
  let doc =
    "URL or path to the Duniverse opam overlays remote that has overrides for packages that have \
     not yet been ported to Dune upstream."
  in
  Arg.(
    value
    & opt string Config.duniverse_overlays_repo
    & info [ "overlay-remote" ] ~docv:"OPAM_REMOTE" ~doc)

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
  let doc =
    Fmt.strf "analyse opam files to generate an initial $(b,%a)" Fpath.pp Config.duniverse_file
  in
  let man = [] in
  Term.info "init" ~doc ~exits ~man

let term =
  let open Term in
  term_result
    ( const run $ Common.Arg.repo $ branch $ explicit_root_packages $ excludes $ pins $ overlay
    $ remotes $ Common.Arg.setup_logs () )

let cmd = (term, info)
