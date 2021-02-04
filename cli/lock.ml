open Import

module Package_argument : sig
  type t

  val make : name:string -> version:OpamPackage.Version.t option -> t

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

let check_root_packages ~local_packages =
  let count = List.length local_packages in
  match count with
  | 0 ->
      Rresult.R.error_msg
        "Cannot find any packages to vendor.\n\
         Either create some *.opam files in the local repository, or specify \
         them manually via 'opam monorepo lock <packages>'."
  | _ ->
      Common.Logs.app (fun l ->
          l "Using %d locally scanned package%a as the root%a." count Pp.plural
            local_packages Pp.plural local_packages);
      Logs.info (fun l ->
          l "Root package%a: %a." Pp.plural local_packages
            Fmt.(list ~sep:(unit ",@ ") Package_argument.pp_styled)
            local_packages);
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

let check_repo_config ~switch_state =
  let repos = current_repos ~switch_state in
  let dune_universe_is_configured = List.exists ~f:is_duniverse_repo repos in
  if not dune_universe_is_configured then
    Logs.warn (fun l ->
        l
          "The dune-universe opam-repository isn't set in the current switch. \
           It contains dune ports for some opam packages. Note that %a will \
           fail if not all of the project dependencies use dune as their build \
           system. Adding this opam-repository to your current switch will \
           help with that. If you wish to do so, run the following command:\n\
           opam repository add dune-universe %s"
          Fmt.(styled `Bold string)
          "opam monorepo lock" Config.duniverse_opam_repo)

let calculate_opam ~build_only ~local_opam_files ~ocaml_version =
  OpamGlobalState.with_ `Lock_none (fun global_state ->
      OpamSwitchState.with_ `Lock_none global_state (fun switch_state ->
          check_repo_config ~switch_state;
          Opam_solve.calculate ~build_only ~local_opam_files ?ocaml_version
            switch_state))

let filter_local_packages ~explicit_list local_paths =
  let res =
    List.fold_left
      ~f:(fun acc pkg ->
        let name = Package_argument.name pkg |> OpamPackage.Name.to_string in
        match (acc, String.Map.find local_paths name) with
        | Error _, Some _ -> acc
        | Error l, None -> Error (name :: l)
        | Ok _, None -> Error [ name ]
        | Ok filtered, Some path ->
            let version = Package_argument.version pkg in
            Ok (String.Map.set filtered name (version, path)))
      ~init:(Ok String.Map.empty) explicit_list
  in
  Result.map_error res ~f:(fun l ->
      let msg =
        Fmt.str "The following packages have no local opam files: %a"
          Fmt.(list ~sep:sp string)
          l
      in
      `Msg msg)

let local_packages ~recurse ~explicit_list repo =
  let open Rresult.R.Infix in
  match explicit_list with
  | [] ->
      Repo.local_packages ~recurse repo >>| fun local_paths ->
      String.Map.map ~f:(fun path -> (None, path)) local_paths
  | _ ->
      Repo.local_packages ~recurse:true
        ~filter:(List.map ~f:Package_argument.name explicit_list)
        repo
      >>= fun local_paths -> filter_local_packages ~explicit_list local_paths

let read_opam fpath =
  let filename =
    OpamFile.make (OpamFilename.of_string (Fpath.to_string fpath))
  in
  Bos.OS.File.with_ic fpath
    (fun ic () -> OpamFile.OPAM.read_from_channel ~filename ic)
    ()

let local_paths_to_opam_map local_paths =
  let open Result.O in
  let bindings = String.Map.bindings local_paths in
  Result.List.map bindings ~f:(fun (name, (explicit_version, path)) ->
      read_opam path >>| fun opam_file ->
      let name = OpamPackage.Name.of_string name in
      let version = Opam.local_package_version opam_file ~explicit_version in
      (name, (version, opam_file)))
  >>| OpamPackage.Name.Map.of_list

let root_depexts local_opam_files =
  OpamPackage.Name.Map.fold
    (fun _pkg (_version, opam_file) acc ->
      OpamFile.OPAM.depexts opam_file :: acc)
    local_opam_files []

let lockfile_path ~explicit_lockfile ~local_packages repo =
  match explicit_lockfile with
  | Some path -> Ok path
  | None ->
      Repo.lockfile
        ~local_packages:(List.map ~f:Package_argument.name local_packages)
        repo

let root_pin_depends local_opam_files =
  OpamPackage.Name.Map.fold
    (fun _pkg (_version, opam_file) acc ->
      OpamFile.OPAM.pin_depends opam_file @ acc)
    local_opam_files []

let pull_pin_depends (pin_depends : (OpamPackage.t * OpamUrl.t) list) =
  (* TODO: Group pin-depends with the same url. *)
  if List.is_empty pin_depends then Ok OpamPackage.Name.Map.empty
  else
    let pins_tmp_dir = Fpath.v "/tmp/opam-monorepo/pins/" in
    OpamGlobalState.with_ `Lock_none (fun global_state ->
        let cache_dir =
          OpamRepositoryPath.download_cache global_state.OpamStateTypes.root
        in
        Logs.debug (fun l ->
            l "Pulling pin depends: %a"
              Fmt.(list ~sep:(unit " ") Fmt.(styled `Yellow string))
              (List.map
                 ~f:(fun (pkg, _) -> OpamPackage.to_string pkg)
                 pin_depends));
        let open Result.O in
        let command (pkg, url) =
          let label = OpamPackage.to_string pkg in
          let out_dir = Fpath.(pins_tmp_dir / label) in
          let open OpamProcess.Job.Op in
          OpamRepository.pull_tree ~cache_dir label
            (OpamFilename.Dir.of_string (Fpath.to_string out_dir))
            [] [ url ]
          @@| function
          | Result _ | Up_to_date _ ->
              let opam_path =
                Fpath.(
                  out_dir / OpamPackage.name_to_string pkg |> add_ext "opam")
              in
              read_opam opam_path >>= fun opam ->
              let opam =
                OpamFile.OPAM.with_url (OpamFile.URL.create url) opam
              in
              Ok (OpamPackage.name pkg, (OpamPackage.version pkg, opam))
          | Not_available (_, long_msg) ->
              Error
                (`Msg (Printf.sprintf "Failed to pull %s: %s" label long_msg))
        in
        let jobs = !OpamStateConfig.r.dl_jobs in
        OpamParallel.map ~jobs ~command pin_depends
        |> Result.List.all >>| OpamPackage.Name.Map.of_list)

let run (`Repo repo) (`Recurse_opam recurse) (`Build_only build_only)
    (`Allow_jbuilder allow_jbuilder) (`Ocaml_version ocaml_version)
    (`Local_packages lp) (`Lockfile explicit_lockfile) () =
  let open Rresult.R.Infix in
  local_packages ~recurse ~explicit_list:lp repo >>= fun local_paths ->
  let local_packages =
    List.map
      ~f:(fun (name, (version, _)) -> Package_argument.make ~name ~version)
      (String.Map.bindings local_paths)
  in
  check_root_packages ~local_packages >>= fun () ->
  local_paths_to_opam_map local_paths >>= fun local_opam_files ->
  let root_pin_depends = root_pin_depends local_opam_files in
  pull_pin_depends root_pin_depends >>= fun pin_opam_files ->
  lockfile_path ~explicit_lockfile ~local_packages repo >>= fun lockfile_path ->
  let pin_packages = OpamPackage.Name.Map.keys pin_opam_files in
  let local_opam_files =
    OpamPackage.Name.Map.union
      (fun _local pin -> pin)
      local_opam_files pin_opam_files
  in
  calculate_opam ~build_only ~allow_jbuilder ~ocaml_version ~local_opam_files
    ~pin_packages
  >>= fun package_summaries ->
  Common.Logs.app (fun l -> l "Calculating exact pins for each of them.");
  compute_duniverse ~package_summaries >>= resolve_ref >>= fun duniverse ->
  let root_packages = String.Map.keys local_paths in
  let root_depexts = root_depexts local_opam_files in
  let lockfile =
    Lockfile.create ~root_packages ~package_summaries ~root_depexts ~duniverse
      ()
  in
  Lockfile.save ~file:lockfile_path lockfile >>= fun () ->
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
    (fun x -> `Local_packages x)
    Arg.(value & pos_all Package_argument.converter [] & info ~doc ~docv [])

let ocaml_version =
  let doc = "Determined version to lock ocaml with in the lockfile." in
  Common.Arg.named
    (fun x -> `Ocaml_version x)
    Arg.(value & opt (some string) None & info ~doc [ "ocaml-version" ])

let info =
  let exits = Term.default_exits in
  let doc =
    Fmt.strf "analyse opam files to generate a project-wide lock file"
  in
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
    (const run $ Common.Arg.repo $ recurse_opam $ build_only $ allow_jbuilder
   $ ocaml_version $ packages $ Common.Arg.lockfile $ Common.Arg.setup_logs ())

let cmd = (term, info)
