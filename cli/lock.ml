open Import
open Types

let check_root_packages ~local_packages =
  match local_packages with
  | [] ->
      Rresult.R.error_msg
        "Cannot find any packages to vendor.\n\
         Either create some *.opam files in the local repository, or specify them manually via \
         'duniverse opam <packages>'."
  | local_packages ->
      let pp_package_name fmt { Opam.name; _ } = Fmt.string fmt name in
      Common.Logs.app (fun l ->
          l "Using locally scanned package%a '%a' as the root%a." Pp.plural local_packages
            Fmt.(list ~sep:(unit ",@ ") (styled `Yellow pp_package_name))
            local_packages Pp.plural local_packages);
      Ok local_packages

let build_config ~local_packages ~pull_mode =
  let open Rresult.R.Infix in
  check_root_packages ~local_packages >>= fun root_packages ->
  let ocaml_compilers =
    match Dune_file.Project.supported_ocaml_compilers () with
    | Ok l -> List.map ~f:Ocaml_version.to_string l
    | Error (`Msg msg) ->
        Logs.warn (fun l -> l "%s" msg);
        []
  in
  let version = "1" in
  let root_packages = Opam.sort_uniq root_packages in
  Ok { Duniverse.Config.version; root_packages; pull_mode; ocaml_compilers }

let compute_deps ~opam_entries =
  Dune_cmd.log_invalid_packages opam_entries;
  let get_default_branch remote = Exec.git_default_branch ~remote () in
  Duniverse.Deps.from_opam_entries ~get_default_branch opam_entries

let resolve_ref deps =
  let resolve_ref ~upstream ~ref = Exec.git_resolve ~remote:upstream ~ref in
  Duniverse.Deps.resolve ~resolve_ref deps

let current_repos ~repo_state ~switch_state =
  let switch_repos = OpamSwitchState.repos_list switch_state in
  List.map ~f:(OpamRepositoryState.get_repo repo_state) switch_repos

let is_duniverse_repo (repo : OpamTypes.repository) =
  let url = OpamUrl.to_string repo.repo_url in
  String.equal url Config.duniverse_opam_repo

let check_repo_config ~global_state ~switch_state =
  OpamRepositoryState.with_ `Lock_none global_state (fun repo_state ->
      let repos = current_repos ~repo_state ~switch_state in
      let dune_universe_is_configured = List.exists ~f:is_duniverse_repo repos in
      if not dune_universe_is_configured then
        Logs.warn (fun l ->
            l
              "The dune-universe opam-repository isn't set in the current switch. It contains dune \
               ports for some opam packages. Note that %a will fail if not all of the project \
               dependencies use dune as their build system. Adding this opam-repository to your \
               current switch will help with that. If you wish to do so, run the following command:\n\
               opam repository add dune-universe %s"
              Fmt.(styled `Bold string)
              "opam monorepo lock" Config.duniverse_opam_repo))

let calculate_opam ~build_only ~local_paths ~local_packages =
  OpamGlobalState.with_ `Lock_none (fun global_state ->
      OpamSwitchState.with_ `Lock_none global_state (fun switch_state ->
          check_repo_config ~global_state ~switch_state;
          Opam_cmd.calculate_opam ~build_only ~local_paths ~local_packages switch_state))

let filter_local_packages ~explicit_list local_paths =
  let res =
    List.fold_left
      ~f:(fun acc { Opam.name; version } ->
        match (acc, String.Map.find local_paths name) with
        | Error _, Some _ -> acc
        | Error l, None -> Error (name :: l)
        | Ok _, None -> Error [ name ]
        | Ok filtered, Some path -> Ok (String.Map.set filtered name (version, path)))
      ~init:(Ok String.Map.empty) explicit_list
  in
  Result.map_error res ~f:(fun l ->
      let msg =
        Fmt.str "The following packages have no local opam files: %a" Fmt.(list ~sep:sp string) l
      in
      `Msg msg)

let local_packages ~recurse ~explicit_list repo =
  let open Rresult.R.Infix in
  match explicit_list with
  | [] ->
      Repo.local_packages ~recurse repo >>| fun local_paths ->
      String.Map.map ~f:(fun path -> (None, path)) local_paths
  | _ ->
      Repo.local_packages ~recurse:true repo >>= fun local_paths ->
      filter_local_packages ~explicit_list local_paths

let run (`Repo repo) (`Pull_mode pull_mode) (`Recurse_opam recurse) (`Build_only build_only)
    (`Local_packages lp) () =
  let open Rresult.R.Infix in
  local_packages ~recurse ~explicit_list:lp repo >>= fun local_paths ->
  let local_packages =
    let open Types.Opam in
    List.map ~f:(fun (name, (version, _)) -> { name; version }) (String.Map.bindings local_paths)
  in
  Repo.duniverse_file ~local_packages repo >>= fun duniverse_file ->
  build_config ~local_packages ~pull_mode >>= fun config ->
  calculate_opam ~build_only ~local_paths ~local_packages >>= fun opam_entries ->
  Opam_cmd.report_packages_stats opam_entries;
  Common.Logs.app (fun l -> l "Calculating Git repositories to vendor source code.");
  compute_deps ~opam_entries >>= resolve_ref >>= fun deps ->
  let duniverse = { Duniverse.config; deps } in
  Duniverse.save ~file:duniverse_file duniverse >>= fun () ->
  Common.Logs.app (fun l ->
      l "Wrote duniverse file with %a entries to %a. You can now run %a to fetch the sources."
        Fmt.(styled `Green int)
        (Duniverse.Deps.count duniverse.deps)
        Pp.Styled.path (Fpath.normalize duniverse_file)
        Fmt.(styled `Blue string)
        "duniverse pull");
  Ok ()

open Cmdliner

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

let recurse_opam =
  let doc =
    "Recursively look for opam files to include as local packages in subdirectories instead of \
     only picking the ones at the repository's root. When an explicit list of local packages is \
     passed, this flag is implied."
  in
  Common.Arg.named (fun x -> `Recurse_opam x) Arg.(value & flag & info ~doc [ "recurse-opam" ])

let build_only =
  let doc = "Only lock build dependencies, i.e. ignore the test deps." in
  Common.Arg.named (fun x -> `Build_only x) Arg.(value & flag & info ~doc [ "build-only" ])

let packages =
  let doc =
    "Explicit list of local packages to compute the lockfile from. When none are provided, all \
     packages that have an opam file at the root of the repository are used."
  in
  let docv = "LOCAL_PACKAGE" in
  Common.Arg.named
    (fun x -> `Local_packages x)
    Arg.(value & pos_all Common.Arg.package [] & info ~doc ~docv [])

let info =
  let exits = Term.default_exits in
  let doc = Fmt.strf "analyse opam files to generate an initial lock file" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command computes a lockfile for all the repository's local packages dependencies and \
         test dependencies.";
      `P
        "All dependencies in the lockfile are pin-depends so that you can install them through \
         opam even if the upstream opam repositories have been modified since you last run \
         $(b,opam monorepo lock).";
      `P
        "Locally set opam repositories and pins will be taken into account. The solver is run \
         everytime you run this command to compute a fixed set of packages meeting the repo's \
         dependencies from scratch. Packages installed in your current switch are simply ignored.";
      `P
        "Since this lockfile must be compatible with $(b,opam monorepo pull) all the dependencies \
         must use dune or jbuilder as their build system. If this requirement isn't met the \
         command will fail.";
    ]
  in
  Term.info "lock" ~doc ~exits ~man

let term =
  let open Term in
  term_result
    ( const run $ Common.Arg.repo $ pull_mode $ recurse_opam $ build_only $ packages
    $ Common.Arg.setup_logs () )

let cmd = (term, info)
