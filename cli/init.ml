open Duniverse_lib
open Duniverse_lib.Types

let build_config ~local_packages ~explicit_root_packages ~pull_mode ~excludes ~opam_repo =
  let open Rresult.R.Infix in
  Opam_cmd.choose_root_packages ~explicit_root_packages ~local_packages >>= fun root_packages ->
  let ocaml_compilers =
    match Dune_file.Project.supported_ocaml_compilers () with
    | Ok l -> List.map Ocaml_version.to_string l
    | Error (`Msg msg) ->
        Logs.warn (fun l -> l "%s" msg);
        []
  in
  let root_packages =
    List.map Opam_cmd.split_opam_name_and_version root_packages |> Opam.sort_uniq
  in
  let excludes =
    List.map Opam_cmd.split_opam_name_and_version (local_packages @ excludes) |> Opam.sort_uniq
  in
  Ok { Duniverse.Config.root_packages; excludes; pull_mode; ocaml_compilers; opam_repo }

let report_stats ~packages =
  let packages_stats = Opam_cmd.packages_stats packages in
  Opam_cmd.report_packages_stats packages_stats

let compute_deps ~opam_entries =
  Dune_cmd.log_invalid_packages opam_entries;
  let get_default_branch remote = Exec.git_default_branch ~remote () in
  Duniverse.Deps.from_opam_entries ~get_default_branch opam_entries

let compute_depexts ~local_opam_repo ~root_packages pkgs =
  let open Rresult.R in
  Exec.map (Opam_cmd.get_opam_depexts ~local_opam_repo ~root_packages) pkgs >>= fun depexts -> Ok (List.flatten depexts)

let resolve_ref deps =
  let resolve_ref ~upstream ~ref = Exec.git_resolve ~remote:upstream ~ref in
  Duniverse.Deps.resolve ~resolve_ref deps

let run (`Repo repo) (`Branch _ (* TODO remove *))
    (`Explicit_root_packages explicit_root_packages) (`Excludes excludes) (`Opam_repo opam_repo)
    (`Pull_mode pull_mode) () =
  let open Rresult.R.Infix in
  let opam_repo = Uri.of_string opam_repo (* TODO move this to cmdliner *) in
  let local_opam_repo = Fpath.v "/Users/avsm/.cache/.duniverse/opam-repository.git" in
  let opam_repo_url = Uri.with_fragment opam_repo None |> Uri.to_string in
  let opam_repo_branch = match Uri.fragment opam_repo with None -> "master" | Some b -> b in
  Exec.git_clone_or_pull ~remote:opam_repo_url ~branch:opam_repo_branch ~output_dir:local_opam_repo
  >>= fun () ->
  Opam_cmd.find_local_opam_packages repo >>= fun local_packages ->
  (* Common.Logs.app (fun l -> l "Local opam packages are: %s" (String.concat ", " local_packages)); *)
  build_config ~local_packages ~explicit_root_packages ~pull_mode ~excludes
    ~opam_repo
  >>= fun config ->
  Common.Logs.app (fun l ->
      l "Resolving opam dependencies for %a"
        Fmt.(list ~sep:(unit " ") Styled_pp.package)
        config.root_packages);
  Opam_cmd.calculate_opam ~config ~local_opam_repo >>= fun packages ->
  report_stats ~packages;
  let depext_pkgs =
    config.root_packages @ List.map (fun { Types.Opam.package; _ } -> package) packages
  in
  Common.Logs.app (fun l ->
      l "Recording depexts for packages %a" Fmt.(list ~sep:(unit " ") Styled_pp.package) depext_pkgs);
  compute_depexts ~local_opam_repo ~root_packages:config.root_packages depext_pkgs >>= fun depexts ->
  List.iter (fun (k, v) -> Common.Logs.app (fun l -> l "%s %s" (String.concat "," k) v)) depexts;
  Common.Logs.app (fun l -> l "Calculating Git repositories to vendor");
  compute_deps ~opam_entries:packages >>= fun unresolved_deps ->
  resolve_ref unresolved_deps >>= fun deps ->
  let duniverse = { Duniverse.config; deps; depexts } in
  let file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.save ~file duniverse >>= fun () ->
  Common.Logs.app (fun l ->
      l "Wrote duniverse file with %a entries to %a."
        Fmt.(styled `Green int)
        (Duniverse.Deps.count duniverse.deps)
        Styled_pp.path (Fpath.normalize file));
  Ok ()

open Cmdliner

let branch =
  let doc = "Branch that represents the working tree of the source code. Defaults to $(i,master)" in
  Common.Arg.named
    (fun x -> `Branch x)
    Cmdliner.Arg.(value & opt string "master" & info [ "b" ] ~docv:"BRANCH" ~doc)

let explicit_root_packages =
  let doc =
    "opam packages to calculate duniverse for. If not supplied, any local opam metadata files are \
     used as the default package list."
  in
  Common.Arg.named
    (fun x -> `Explicit_root_packages x)
    Arg.(value & pos_all string [] & info [] ~doc ~docv:"PACKAGES")

let excludes =
  let doc =
    "Packages to exclude from the output list. You can use this to remove the root packages so \
     they are not duplicated in the vendor directory.  Repeat this flag multiple times for more \
     than one exclusion."
  in
  Common.Arg.named
    (fun x -> `Excludes x)
    Arg.(value & opt_all string [] & info [ "exclude"; "x" ] ~docv:"EXCLUDE" ~doc)

let opam_repo =
  let doc =
    "URL or path to the Duniverse opam-repository that has overrides for packages that have not \
     yet been ported to Dune upstream."
  in
  Common.Arg.named
    (fun x -> `Opam_repo x)
    Arg.(
      value & opt string Config.duniverse_opam_repo & info [ "opam-repo" ] ~docv:"OPAM_REPO" ~doc)

let pull_mode =
  let doc =
    "How to pull the sources. If $(i,submodules), the pull command will initialise them as git \
     submodules.  If $(i,source) then the source code will directly be cloned to the source tree."
  in
  Common.Arg.named
    (fun x -> `Pull_mode x)
    Arg.(
      value
      & opt
          (enum [ ("submodule", Duniverse.Config.Submodules); ("source", Duniverse.Config.Source) ])
          Duniverse.Config.Source
      & info [ "pull-mode" ] ~docv:"PULL_MODE" ~doc)

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
    ( const run $ Common.Arg.repo $ branch $ explicit_root_packages $ excludes $ opam_repo
    $ pull_mode $ Common.Arg.setup_logs () )

let cmd = (term, info)
