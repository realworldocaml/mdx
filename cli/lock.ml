open Import

module Package_argument : sig
  type t

  val name : t -> OpamPackage.Name.t

  val version : t -> OpamPackage.Version.t option

  val converter : t Cmdliner.Arg.converter

  val pp_styled : t Fmt.t
end = struct
  type t = { name : OpamPackage.Name.t; version : OpamPackage.Version.t option }

  let make ~name ~version =
    let name = OpamPackage.Name.of_string name in
    { name; version }

  let name { name; _ } = name

  let version { version; _ } = version

  let from_string name =
    match Astring.String.cut ~sep:"." name with
    | None -> make ~name ~version:None
    | Some (name, version) ->
        make ~name ~version:(Some (OpamPackage.Version.of_string version))

  let pp_version_opt ppf =
    Option.iter (fun version ->
        Fmt.pf ppf ".%s" (OpamPackage.Version.to_string version))

  let pp ppf { name; version } =
    Fmt.pf ppf "%s%a" (OpamPackage.Name.to_string name) pp_version_opt version

  let converter =
    let parse s = Ok (from_string s) in
    Cmdliner.Arg.conv ~docv:"PACKAGE" (parse, pp)

  let pp_styled ppf t = Fmt.to_to_string pp t |> Pp.Styled.package_name ppf
end

let check_target_packages packages =
  let count = OpamPackage.Name.Set.cardinal packages in
  let set ?sep pp_elt = Fmt.iter ?sep OpamPackage.Name.Set.iter pp_elt in
  match count with
  | 0 ->
      Rresult.R.error_msg
        "Cannot find any packages to vendor.\n\
         Either create some *.opam files in the local repository, or specify \
         them manually via 'opam monorepo lock <packages>'."
  | _ ->
      Common.Logs.app (fun l ->
          l "Using %d locally scanned package%a as the target%a." count
            Pp.plural_int count Pp.plural_int count);
      Logs.info (fun l ->
          l "Target package%a: %a." Pp.plural_int count
            Fmt.(set ~sep:(any ",@ ") Opam.Pp.package_name)
            packages);
      Ok ()

let opam_to_git_remote remote =
  match String.lsplit2 ~on:'+' remote with
  | Some ("git", remote) -> remote
  | _ -> remote

let compute_duniverse ~package_summaries =
  let get_default_branch remote =
    Exec.git_default_branch ~remote:(opam_to_git_remote remote) ()
  in
  Duniverse.from_package_summaries ~get_default_branch package_summaries

let resolve_ref deps =
  let resolve_ref ~repo ~ref =
    Exec.git_resolve ~remote:(opam_to_git_remote repo) ~ref
  in
  Duniverse.resolve ~resolve_ref deps

let current_repos ~switch_state =
  let repo_state = switch_state.OpamStateTypes.switch_repos in
  let switch_repos = OpamSwitchState.repos_list switch_state in
  List.map ~f:(OpamRepositoryState.get_repo repo_state) switch_repos

let is_duniverse_repo (repo : OpamTypes.repository) =
  let url = OpamUrl.to_string repo.repo_url in
  String.equal url Config.duniverse_opam_repo

let check_dune_universe_repo ~switch_state non_dune_packages =
  let repos = current_repos ~switch_state in
  let dune_universe_is_configured = List.exists ~f:is_duniverse_repo repos in
  if not dune_universe_is_configured then
    Logs.warn (fun l ->
        l
          "Couldn't calculate a set of packages to satisfy the request. Note \
           that %a will fail if not all of the project dependencies use dune \
           as their build system, in your project that would be %a. To solve \
           this issue there exists a dune-universe opam-repository which \
           contains dune ports for some opam packages, but it is currently not \
           set in your current switch. If you wish to set it up, run the \
           following command:\n\
           opam repository add dune-universe %s"
          Fmt.(styled `Bold string)
          "opam monorepo lock"
          Fmt.(list ~sep:comma Opam.Pp.package_name)
          non_dune_packages Config.duniverse_opam_repo)

let read_opam fpath =
  let filename =
    OpamFile.make (OpamFilename.of_string (Fpath.to_string fpath))
  in
  Bos.OS.File.with_ic fpath
    (fun ic () -> OpamFile.OPAM.read_from_channel ~filename ic)
    ()

let local_paths_to_opam_map local_paths =
  let open Result.O in
  let bindings = OpamPackage.Name.Map.bindings local_paths in
  Result.List.map bindings ~f:(fun (name, (explicit_version, path)) ->
      let+ opam_file = read_opam path in
      let version = Opam.local_package_version opam_file ~explicit_version in
      (name, (version, opam_file)))
  >>| OpamPackage.Name.Map.of_list

let target_depexts opam_files target_packages =
  OpamPackage.Name.Map.fold
    (fun pkg_name (_version, opam_file) acc ->
      match OpamPackage.Name.Set.mem pkg_name target_packages with
      | false -> acc
      | true -> OpamFile.OPAM.depexts opam_file :: acc)
    opam_files []

let lockfile_path ~explicit_lockfile ~target_packages repo =
  match explicit_lockfile with
  | Some path -> Ok path
  | None ->
      Project.lockfile
        ~target_packages:(OpamPackage.Name.Set.elements target_packages)
        repo
      |> Result.map_error ~f:(function `Msg msg ->
             Rresult.R.msgf
               "Could not infer the target lockfile name: %s\n\
                Try setting it explicitly using --lockfile or add a project \
                name in a root dune-project file."
               msg)

let root_pin_depends local_opam_files =
  OpamPackage.Name.Map.fold
    (fun _pkg (_version, opam_file) acc ->
      OpamFile.OPAM.pin_depends opam_file @ acc)
    local_opam_files []
  |> Pin_depends.sort_uniq

let pull_pin_depends ~global_state pin_depends =
  let open Result.O in
  if List.is_empty pin_depends then Ok OpamPackage.Name.Map.empty
  else
    let* pins_tmp_dir = Bos.OS.Dir.tmp "opam-monorepo-pins-%s" in
    Logs.debug (fun l ->
        l "Pulling pin depends: %a"
          Fmt.(list ~sep:(any " ") Fmt.(styled `Yellow string))
          (List.map ~f:(fun (pkg, _) -> OpamPackage.to_string pkg) pin_depends));
    let by_urls = OpamUrl.Map.bindings (Pin_depends.group_by_url pin_depends) in
    let elm_from_pkg ~dir ~url pkg =
      let opam_path =
        Fpath.(dir / OpamPackage.name_to_string pkg |> add_ext "opam")
      in
      let* opam = read_opam opam_path in
      let opam = OpamFile.OPAM.with_url (OpamFile.URL.create url) opam in
      Ok (OpamPackage.name pkg, (OpamPackage.version pkg, opam))
    in
    let command (url, pkgs) =
      let label = Filename.remove_extension (OpamUrl.basename url) in
      let dir = Fpath.(pins_tmp_dir / label) in
      let open OpamProcess.Job.Op in
      Opam.pull_tree ~url ~hashes:[] ~dir global_state @@| fun result ->
      let* () = result in
      Result.List.map ~f:(elm_from_pkg ~dir ~url) pkgs
    in
    let jobs = !OpamStateConfig.r.dl_jobs in
    let+ elms = OpamParallel.map ~jobs ~command by_urls |> Result.List.all in
    OpamPackage.Name.Map.of_list (List.concat elms)

let get_pin_depends ~global_state local_opam_files =
  let open Result.O in
  root_pin_depends local_opam_files >>= pull_pin_depends ~global_state

let display_verbose_diagnostics = function
  | None -> false
  | Some l -> l >= Logs.Info

let interpret_solver_error ~switch_state solver = function
  | `Msg _ as err -> err
  | `Diagnostics d ->
      (match Opam_solve.not_buildable_with_dune solver d with
      | [] -> ()
      | offending_packages ->
          check_dune_universe_repo ~switch_state offending_packages);
      let verbose = display_verbose_diagnostics (Logs.level ()) in
      Opam_solve.diagnostics_message ~verbose solver d

let calculate_opam ~build_only ~allow_jbuilder ~local_opam_files ~ocaml_version
    ~target_packages =
  let open Result.O in
  OpamGlobalState.with_ `Lock_none (fun global_state ->
      OpamSwitchState.with_ `Lock_none global_state (fun switch_state ->
          let solver = Opam_solve.local_opam_config_solver in
          let* pin_depends = get_pin_depends ~global_state local_opam_files in
          Opam_solve.calculate ~build_only ~allow_jbuilder ~local_opam_files
            ~target_packages ~pin_depends ?ocaml_version solver switch_state
          |> Result.map_error ~f:(interpret_solver_error ~switch_state solver)))

let select_explicitly_specified ~local_packages ~explicitly_specified =
  List.fold_left
    ~f:(fun acc specified ->
      let key = Package_argument.name specified in
      match (OpamPackage.Name.Map.mem key local_packages, acc) with
      | false, Ok _ -> Error [ specified ]
      | false, Error errors -> Error (specified :: errors)
      | true, Error errors -> Error errors
      | true, Ok selected -> Ok (OpamPackage.Name.Set.add key selected))
    ~init:(Ok OpamPackage.Name.Set.empty) explicitly_specified
  |> Result.map_error ~f:(fun missing_packages ->
         let msg =
           Fmt.str "Package%a %a specified but not found in repository"
             Pp.plural missing_packages
             Fmt.(list ~sep:comma Package_argument.pp_styled)
             missing_packages
         in
         `Msg msg)

let filter_root_packages root packages =
  OpamPackage.Name.Map.filter
    (fun _key (_, path) ->
      match Fpath.rem_prefix root path with
      | None ->
          (* `packages` contains a path not within `root`, this shouldn't happen and is a bug *)
          assert false
      | Some path ->
          let filename = path |> Fpath.filename |> Fpath.v in
          Fpath.equal filename path)
    packages

let warn_duplicate_paths ~packages duplicates =
  let pp_path_entry fmt = Fmt.pf fmt "- %a" Fpath.pp in
  let pp_paths = Fmt.(list ~sep:(any "\n") pp_path_entry) in
  let pp_package fmt (name, duplicate_paths, taken) =
    Fmt.pf fmt
      "Package %a is defined multiple times in the repository:\n\
       %a\n\
       We kept %a and discarded the others" Opam.Pp.package_name name pp_paths
      duplicate_paths Fpath.pp taken
  in
  OpamPackage.Name.Map.iter
    (fun name duplicate_paths ->
      let _, selected_path = OpamPackage.Name.Map.find name packages in
      Logs.warn (fun l ->
          l "%a" pp_package (name, duplicate_paths, selected_path)))
    duplicates

let package_version_map ~versions packages =
  let versions =
    List.fold_left
      ~f:(fun acc pkg_arg ->
        match pkg_arg |> Package_argument.version with
        | None -> acc
        | Some pkg_version ->
            let pkg_name = pkg_arg |> Package_argument.name in
            OpamPackage.Name.Map.add pkg_name pkg_version acc)
      ~init:OpamPackage.Name.Map.empty versions
  in
  let rec loop (packages, duplicates) = function
    | [] -> (packages, duplicates)
    | (pkg_name, path) :: xs -> (
        match OpamPackage.Name.Map.find_opt pkg_name packages with
        | Some (_, existing_path) ->
            let dups =
              match OpamPackage.Name.Map.find_opt pkg_name duplicates with
              | None -> [ path; existing_path ]
              | Some paths -> path :: paths
            in
            let duplicates =
              OpamPackage.Name.Map.add pkg_name dups duplicates
            in
            loop (packages, duplicates) xs
        | None ->
            let pkg_version = OpamPackage.Name.Map.find_opt pkg_name versions in
            let packages =
              OpamPackage.Name.Map.add pkg_name (pkg_version, path) packages
            in
            loop (packages, duplicates) xs)
  in
  let packages, duplicates =
    loop OpamPackage.Name.Map.(empty, empty) packages
  in
  warn_duplicate_paths ~packages duplicates;
  packages

let package_names m =
  m |> OpamPackage.Name.Map.keys |> OpamPackage.Name.Set.of_list

let target_packages ~local_packages ~recurse ~explicitly_specified repo =
  match explicitly_specified with
  | [] when recurse -> local_packages |> package_names |> Result.ok
  | [] ->
      local_packages |> filter_root_packages repo |> package_names |> Result.ok
  | _ -> select_explicitly_specified ~local_packages ~explicitly_specified

let local_packages ~versions repo =
  let open Result.O in
  Project.all_local_packages repo >>| package_version_map ~versions

let run (`Root root) (`Recurse_opam recurse) (`Build_only build_only)
    (`Allow_jbuilder allow_jbuilder) (`Ocaml_version ocaml_version)
    (`Target_packages specified_packages) (`Lockfile explicit_lockfile) () =
  let open Result.O in
  let* local_packages = local_packages ~versions:specified_packages root in
  let* target_packages =
    target_packages ~local_packages ~recurse
      ~explicitly_specified:specified_packages root
  in
  let* () = check_target_packages target_packages in
  let* opam_files = local_paths_to_opam_map local_packages in
  let* lockfile_path = lockfile_path ~explicit_lockfile ~target_packages root in
  let* package_summaries =
    calculate_opam ~build_only ~allow_jbuilder ~ocaml_version
      ~local_opam_files:opam_files ~target_packages
  in
  Common.Logs.app (fun l -> l "Calculating exact pins for each of them.");
  let* duniverse = compute_duniverse ~package_summaries >>= resolve_ref in
  let target_depexts = target_depexts opam_files target_packages in
  let lockfile =
    Lockfile.create ~root_packages:target_packages ~package_summaries
      ~root_depexts:target_depexts ~duniverse ()
  in
  let* () = Lockfile.save ~file:lockfile_path lockfile in
  Common.Logs.app (fun l ->
      l
        "Wrote lockfile with %a entries to %a. You can now run %a to fetch \
         their sources."
        Fmt.(styled `Green int)
        (List.length duniverse) Pp.Styled.path
        (Fpath.normalize lockfile_path)
        Fmt.(styled `Blue string)
        "opam monorepo pull");
  Ok ()

open Cmdliner

let recurse_opam =
  let doc =
    "Recursively look for opam files to include as local packages in \
     subdirectories instead of only picking the ones at the repository's root. \
     When an explicit list of local packages is passed, this flag is implied."
  in
  Common.Arg.named
    (fun x -> `Recurse_opam x)
    Arg.(value & flag & info ~doc [ "recurse-opam" ])

let build_only =
  let doc = "Only lock build dependencies, i.e. ignore the test deps." in
  Common.Arg.named
    (fun x -> `Build_only x)
    Arg.(value & flag & info ~doc [ "build-only" ])

let allow_jbuilder =
  let doc =
    "Include packages depending on `jbuilder` for the resolution. Please note \
     that since dune 2.0, `jbuild` files are not supported: the files will \
     need to be upgraded manually."
  in
  Common.Arg.named
    (fun x -> `Allow_jbuilder x)
    Arg.(value & flag & info ~doc [ "allow-jbuilder" ])

let packages =
  let doc =
    "Explicit list of local packages to compute the lockfile from. These can \
     be either plain package names (such as \"dune-release\"), or packages \
     with a version number (e.g. \"irmin.2.7.1\"). Version numbers are used to \
     tell the solver what is the version of the local package. When none are \
     provided, all packages that have an opam file at the root of the \
     repository are used."
  in
  let docv = "LOCAL_PACKAGE" in
  Common.Arg.named
    (fun x -> `Target_packages x)
    Arg.(value & pos_all Package_argument.converter [] & info ~doc ~docv [])

let ocaml_version =
  let doc = "Determined version to lock ocaml with in the lockfile." in
  Common.Arg.named
    (fun x -> `Ocaml_version x)
    Arg.(value & opt (some string) None & info ~doc [ "ocaml-version" ])

let info =
  let exits = Term.default_exits in
  let doc = Fmt.str "analyse opam files to generate a project-wide lock file" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command computes a lockfile for all the repository's local \
         packages dependencies and test dependencies.";
      `P
        "All dependencies in the lockfile are pin-depends so that you can \
         install them through opam even if the upstream opam repositories have \
         been modified since you last run $(b,opam monorepo lock).";
      `P
        "Locally set opam repositories and pins will be taken into account. \
         The solver is run everytime you run this command to compute a fixed \
         set of packages meeting the repo's dependencies from scratch. \
         Packages installed in your current switch are simply ignored.";
      `P
        "Since this lockfile must be compatible with $(b,opam monorepo pull) \
         all the dependencies must use dune or jbuilder as their build system. \
         If this requirement isn't met the command will fail. We maintain an \
         opam repository with dune port of opam packages. We suggest you add \
         it to your switch's repositories before running $(b, opam monorepo \
         lock) if you know some of your dependencies don't use dune. If some \
         of them haven't been ported yet, please head to \
         dune-universe/opam-overlays on github.com. Feel free to follow the \
         instructions there to add dune ports for the packages you need.";
    ]
  in
  Term.info "lock" ~doc ~exits ~man

let term =
  let open Term in
  term_result
    (const run $ Common.Arg.root $ recurse_opam $ build_only $ allow_jbuilder
   $ ocaml_version $ packages $ Common.Arg.lockfile $ Common.Arg.setup_logs ())

let cmd = (term, info)
