open Import

let min_dune_ver = Dune_file.Lang.duniverse_minimum_version

let update_lang ~content =
  List.map content ~f:(fun line ->
      if Dune_file.Lang.is_stanza line then Dune_file.Raw.duniverse_minimum_lang else line)

let should_update_lang ~yes () =
  Prompt.confirm ~question:(fun l -> l "Should I update your dune-project?") ~yes

let log_version_update ~dune_project_path =
  Common.Logs.app (fun l ->
      l "Setting dune language version to %a in %a" Dune_file.Lang.pp_version min_dune_ver
        Pp.Styled.path dune_project_path)

let suggest_updating_version ~yes ~version ~dune_project_path ~content =
  let pp_current = Pp.Styled.bad Dune_file.Lang.pp_version in
  let pp_required = Pp.Styled.good Dune_file.Lang.pp_version in
  Common.Logs.app (fun l -> l "You are using version %a of the dune language" pp_current version);
  Common.Logs.app (fun l -> l "Duniverse requires version %a or above" pp_required min_dune_ver);
  if should_update_lang ~yes () then (
    let updated = update_lang ~content @ [ "" ] in
    log_version_update ~dune_project_path;
    Bos.OS.File.write_lines dune_project_path updated )
  else Ok ()

let suggest_setting_version ~yes ~dune_project_path ~content =
  Common.Logs.app (fun l -> l "Your dune-project file doesn't specify a dune language version");
  if should_update_lang ~yes () then (
    let updated = Dune_file.Raw.duniverse_minimum_lang :: content in
    log_version_update ~dune_project_path;
    Persist.write_lines_hum dune_project_path updated )
  else Ok ()

let check_dune_lang_version ~yes ~repo =
  let open Result.O in
  let dune_project_path = Fpath.(repo / "dune-project") in
  Logs.debug (fun l -> l "Looking for dune-project file in %a" Pp.Styled.path dune_project_path);
  Bos.OS.File.exists dune_project_path >>= fun found_dune_project ->
  if found_dune_project then
    Bos.OS.File.read_lines dune_project_path >>= fun content ->
    let lang_stanza = List.find_opt ~f:Dune_file.Lang.is_stanza content in
    match lang_stanza with
    | None -> suggest_setting_version ~yes ~dune_project_path ~content
    | Some s -> (
        Dune_file.Lang.parse_stanza s >>= fun version ->
        let compared = Dune_file.Lang.(compare_version version duniverse_minimum_version) in
        match Ordering.of_int compared with
        | Eq | Gt -> Ok ()
        | Lt -> suggest_updating_version ~yes ~version ~dune_project_path ~content )
  else (
    Logs.debug (fun l -> l "No dune-project found");
    Ok () )

let run (`Yes yes) (`No_cache no_cache) (`Repo repo) (`Duniverse_repos duniverse_repos) () =
  let open Result.O in
  Repo.duniverse_file repo >>= fun duniverse_file ->
  Duniverse.load ~file:duniverse_file >>= function
  | { deps = { duniverse = []; _ }; _ } ->
      Common.Logs.app (fun l -> l "No dependencies to pull, there's nothing to be done here!");
      Ok ()
  | { deps = { duniverse; _ }; config; _ } ->
      Common.filter_duniverse ~to_consider:duniverse_repos duniverse >>= fun duniverse ->
      Common.Logs.app (fun l ->
          l "Using pull mode %s"
            (Sexplib.Sexp.to_string_hum Duniverse.Config.(sexp_of_pull_mode config.pull_mode)));
      check_dune_lang_version ~yes ~repo >>= fun () ->
      Common.get_cache ~no_cache >>= fun cache ->
      Pull.duniverse ~cache ~pull_mode:config.Duniverse.Config.pull_mode ~repo duniverse

let info =
  let open Cmdliner in
  let doc = "fetch the latest archives of the vendored libraries" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command reads the Git metadata calculated with $(i,duniverse lock) and fetches them \
         from their respective Git remotes and stores them in the $(b,duniverse/) directory in the \
         repository.";
      `P
        "This command uses a global duniverse cache to avoid repeated downloads. To determine \
         where the cache should be located it reads a few environment variables. If none of those \
         are set, a warning will be displayed and the cache will be disabled. To learn more about \
         which variables are used and their priority go to the $(b,ENVIRONMENT) section. Note that \
         you can also manually disable the cache using the $(b,--no-cache) CLI flag documented in \
         the $(b,OPTIONS) section below.";
    ]
  in
  Term.info "pull" ~doc ~exits ~man ~envs:Common.Arg.caches

let term =
  Cmdliner.Term.(
    term_result
      ( const run $ Common.Arg.yes $ Common.Arg.no_cache $ Common.Arg.repo
      $ Common.Arg.duniverse_repos $ Common.Arg.setup_logs () ))

let cmd = (term, info)
