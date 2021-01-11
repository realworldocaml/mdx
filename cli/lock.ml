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
      Ok ()

let compute_duniverse ~package_summaries =
  let get_default_branch remote = Exec.git_default_branch ~remote () in
  Duniverse.from_package_summaries ~get_default_branch package_summaries

let resolve_ref deps =
  let resolve_ref ~repo ~ref =
    let repo = match String.lsplit2 ~on:'+' repo with Some ("git", repo) -> repo | _ -> repo in
    Exec.git_resolve ~remote:repo ~ref
  in
  Duniverse.resolve ~resolve_ref deps

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

let calculate_opam ~build_only ~local_opam_files ~local_packages =
  OpamGlobalState.with_ `Lock_none (fun global_state ->
      OpamSwitchState.with_ `Lock_none global_state (fun switch_state ->
          check_repo_config ~global_state ~switch_state;
          Opam_solve.calculate ~build_only ~local_opam_files ~local_packages switch_state))

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
      Repo.local_packages ~recurse:true ~filter:explicit_list repo >>= fun local_paths ->
      filter_local_packages ~explicit_list local_paths

let read_opam fpath =
  let filename = OpamFile.make (OpamFilename.of_string (Fpath.to_string fpath)) in
  Bos.OS.File.with_ic fpath (fun ic () -> OpamFile.OPAM.read_from_channel ~filename ic) ()

let local_paths_to_opam_map local_paths =
  let open Result.O in
  let bindings = String.Map.bindings local_paths in
  Result.List.map bindings ~f:(fun (name, (version, path)) ->
      read_opam path >>| fun opam_file ->
      let name = OpamPackage.Name.of_string name in
      let version =
        OpamPackage.Version.of_string (Option.value ~default:Types.Opam.default_version version)
      in
      (name, (version, opam_file)))
  >>| OpamPackage.Name.Map.of_list

let root_depexts local_opam_files =
  OpamPackage.Name.Map.fold
    (fun _pkg (_version, opam_file) acc -> OpamFile.OPAM.depexts opam_file :: acc)
    local_opam_files []

let run (`Repo repo) (`Recurse_opam recurse) (`Build_only build_only) (`Local_packages lp) () =
  let open Rresult.R.Infix in
  local_packages ~recurse ~explicit_list:lp repo >>= fun local_paths ->
  let local_packages =
    let open Types.Opam in
    List.map ~f:(fun (name, (version, _)) -> { name; version }) (String.Map.bindings local_paths)
  in
  check_root_packages ~local_packages >>= fun () ->
  local_paths_to_opam_map local_paths >>= fun local_opam_files ->
  Repo.lockfile ~local_packages repo >>= fun lockfile_path ->
  calculate_opam ~build_only ~local_opam_files ~local_packages >>= fun package_summaries ->
  Common.Logs.app (fun l -> l "Calculating exact pins for each of them.");
  compute_duniverse ~package_summaries >>= resolve_ref >>= fun duniverse ->
  let root_packages = String.Map.keys local_paths in
  let root_depexts = root_depexts local_opam_files in
  let lockfile = Lockfile.create ~root_packages ~package_summaries ~root_depexts ~duniverse () in
  Lockfile.save ~file:lockfile_path lockfile >>= fun () ->
  Common.Logs.app (fun l ->
      l "Wrote lockfile with %a entries to %a. You can now run %a to fetch their sources."
        Fmt.(styled `Green int)
        (List.length duniverse) Pp.Styled.path (Fpath.normalize lockfile_path)
        Fmt.(styled `Blue string)
        "opam monorepo pull");
  Ok ()

open Cmdliner

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
  let doc = Fmt.strf "analyse opam files to generate a project-wide lock file" in
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
         command will fail. We maintain an opam repository with dune port of opam packages. We \
         suggest you add it to your switch's repositories before running $(b, opam monorepo lock) \
         if you know some of your dependencies don't use dune. If some of them haven't been ported \
         yet, please head to dune-universe/opam-overlays on github.com. Feel free to follow the \
         instructions there to add dune ports for the packages you need.";
    ]
  in
  Term.info "lock" ~doc ~exits ~man

let term =
  let open Term in
  term_result
    (const run $ Common.Arg.repo $ recurse_opam $ build_only $ packages $ Common.Arg.setup_logs ())

let cmd = (term, info)
