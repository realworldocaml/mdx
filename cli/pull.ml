open Import

let min_dune_ver = Dune_file.Lang.duniverse_minimum_version

let should_update_lang ~yes () =
  Prompt.confirm
    ~question:(fun l -> l "Should I update your dune-project?")
    ~yes

let log_version_update ~dune_project_path =
  Common.Logs.app (fun l ->
      l "Setting dune language version to %a in %a" Dune_file.Lang.pp_version
        min_dune_ver Pp.Styled.path dune_project_path)

let suggest_updating_version ~yes ~version ~dune_project_path ~content =
  let pp_current = Pp.Styled.bad Dune_file.Lang.pp_version in
  let pp_required = Pp.Styled.good Dune_file.Lang.pp_version in
  Common.Logs.app (fun l ->
      l "You are using version %a of the dune language" pp_current version);
  Common.Logs.app (fun l ->
      l "Duniverse requires version %a or above" pp_required min_dune_ver);
  if should_update_lang ~yes () then (
    let updated = Dune_file.Lang.update ~version:min_dune_ver content in
    log_version_update ~dune_project_path;
    Bos.OS.File.write dune_project_path updated)
  else Ok ()

let check_dune_lang_version ~yes ~repo =
  let open Result.O in
  let dune_project_path = Fpath.(repo / "dune-project") in
  Logs.debug (fun l ->
      l "Looking for dune-project file in %a" Pp.Styled.path dune_project_path);
  Bos.OS.File.exists dune_project_path >>= fun found_dune_project ->
  if found_dune_project then
    Bos.OS.File.read dune_project_path >>= fun content ->
    match Dune_file.Lang.from_content content with
    | Error (`Msg msg) ->
        Logs.warn (fun l -> l "%s" msg);
        Ok ()
    | Ok version -> (
        let compared =
          Dune_file.Lang.(compare_version version duniverse_minimum_version)
        in
        match Ordering.of_int compared with
        | Eq | Gt -> Ok ()
        | Lt ->
            suggest_updating_version ~yes ~version ~dune_project_path ~content)
  else (
    Logs.debug (fun l -> l "No dune-project found");
    Ok ())

let run (`Yes yes) (`Repo repo) (`Lockfile explicit_lockfile)
    (`Duniverse_repos duniverse_repos) () =
  let open Result.O in
  Common.find_lockfile ~explicit_lockfile repo >>= fun lockfile ->
  Lockfile.to_duniverse lockfile >>= function
  | [] ->
      Common.Logs.app (fun l ->
          l "No dependencies to pull, there's nothing to be done here!");
      Ok ()
  | duniverse ->
      let full = match duniverse_repos with None -> true | _ -> false in
      Common.filter_duniverse ~to_consider:duniverse_repos duniverse
      >>= fun duniverse ->
      check_dune_lang_version ~yes ~repo >>= fun () ->
      OpamGlobalState.with_ `Lock_none (fun global_state ->
          Pull.duniverse ~global_state ~repo ~full duniverse)

let info =
  let open Cmdliner in
  let doc = "fetch the dependencies sources as specified by the lockfile" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command fetches the sources of the dependencies according to the \
         lockfile calculated with $(b,opam monorepo lock), stores them in the \
         $(b,duniverse/) directory in the repository and set it up so they are \
         treated as vendored code by dune.";
      `P
        "The previous content of the $(b,duniverse/) folder is deleted upon \
         calling this command, unless a subset of repositories to pull is \
         explicitly passed on the command line.";
    ]
  in
  Term.info "pull" ~doc ~exits ~man

let term =
  Cmdliner.Term.(
    term_result
      (const run $ Common.Arg.yes $ Common.Arg.repo $ Common.Arg.lockfile
     $ Common.Arg.duniverse_repos $ Common.Arg.setup_logs ()))

let cmd = (term, info)
