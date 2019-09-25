module Fmt_ext = Fmt
open Stdune
open Duniverse_lib
open Rresult

let min_dune_ver = Dune_file.Lang.duniverse_minimum_version

let update_lang ~content =
  List.map content ~f:(fun line ->
      if Dune_file.Lang.is_stanza line then Dune_file.Raw.duniverse_minimum_lang else line )

let should_update_lang ~yes () =
  Prompt.confirm ~question:(fun l -> l "Should I update your dune-project?") ~yes

let log_version_update ~dune_project_path =
  Common.Logs.app (fun l ->
      l "Setting dune language version to %a in %a" Dune_file.Lang.pp_version min_dune_ver
        Styled_pp.path dune_project_path )

let suggest_updating_version ~yes ~version ~dune_project_path ~content =
  let pp_current = Styled_pp.bad Dune_file.Lang.pp_version in
  let pp_required = Styled_pp.good Dune_file.Lang.pp_version in
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
  Logs.debug (fun l -> l "Looking for dune-project file in %a" Styled_pp.path dune_project_path);
  Bos.OS.File.exists dune_project_path >>= fun found_dune_project ->
  if found_dune_project then
    Bos.OS.File.read_lines dune_project_path >>= fun content ->
    let lang_stanza = List.find ~f:Dune_file.Lang.is_stanza content in
    match lang_stanza with
    | None -> suggest_setting_version ~yes ~dune_project_path ~content
    | Some s -> (
        Dune_file.Lang.parse_stanza s >>= fun version ->
        match Dune_file.Lang.(compare_version version duniverse_minimum_version) with
        | Eq | Gt -> Ok ()
        | Lt -> suggest_updating_version ~yes ~version ~dune_project_path ~content )
  else (
    Logs.debug (fun l -> l "No dune-project found");
    Ok () )

let mark_duniverse_content_as_vendored ~duniverse_dir =
  let open Result.O in
  let dune_file = Fpath.(duniverse_dir / "dune") in
  let content = Dune_file.Raw.duniverse_dune_content in
  Logs.debug (fun l ->
      l "Writing %a:\n %s" Styled_pp.path dune_file (String.concat ~sep:"\n" content) );
  Persist.write_lines_hum dune_file content >>= fun () ->
  Logs.debug (fun l -> l "Successfully wrote %a" Styled_pp.path dune_file);
  Ok ()

let pull ~duniverse_dir ~cache src_dep =
  let open Result.O in
  let open Duniverse.Deps.Source in
  let { dir; upstream; ref = { Git.Ref.t = ref; commit }; _ } = src_dep in
  let output_dir = Fpath.(duniverse_dir / dir) in
  Bos.OS.Dir.delete ~recurse:true output_dir >>= fun () ->
  Cloner.clone_to ~output_dir ~remote:upstream ~ref ~commit cache
  |> Rresult.R.reword_error (fun (`Msg _) -> `Commit_is_gone dir)
  >>= fun cached ->
  Common.Logs.app (fun l ->
      l "Pulled sources for %a.%a" Styled_pp.path output_dir Styled_pp.cached cached );
  Bos.OS.Dir.delete ~must_exist:true ~recurse:true Fpath.(output_dir / ".git") >>= fun () ->
  Bos.OS.Dir.delete ~recurse:true Fpath.(output_dir // Config.vendor_dir)

let report_commit_is_gone_repos repos =
  let sep fmt () =
    Format.pp_print_newline fmt ();
    Styled_pp.header_indent fmt ();
    Fmt_ext.(const string "  - ") fmt ()
  in
  let fmt_repos = Fmt_ext.(list ~sep Styled_pp.package_name) in
  Common.Logs.app (fun l ->
      l "The following repos could not be pulled as the commit we want is gone:%a%a" sep ()
        fmt_repos repos );
  Common.Logs.app (fun l ->
      l "You should run 'duniverse update' to fix the commits associated with the tracked refs" )

let pull_source_dependencies ~duniverse_dir ~cache src_deps =
  let open Result.O in
  Parallel.map ~f:(pull ~duniverse_dir ~cache) src_deps
  |> Result.List.fold_left ~init:[] ~f:(fun acc res ->
         match res with
         | Ok () -> Ok acc
         | Error (`Commit_is_gone dir) -> Ok (dir :: acc)
         | Error (`Msg _ as err) -> Error (err :> [> `Msg of string ]) )
  >>= function
  | [] ->
      let total = List.length src_deps in
      let pp_count = Styled_pp.good Fmt_ext.int in
      Common.Logs.app (fun l ->
          l "Successfully pulled %a/%a repositories" pp_count total pp_count total );
      Ok ()
  | commit_is_gone_repos ->
      report_commit_is_gone_repos commit_is_gone_repos;
      Error (`Msg "Could not pull all the source dependencies")

let get_cache ~no_cache = if no_cache then Ok Cloner.no_cache else Cloner.get_cache ()

let run (`Yes yes) (`No_cache no_cache) (`Repo repo) () =
  let open Result.O in
  let duniverse_file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.load ~file:duniverse_file >>= function
  | { deps = { duniverse = []; _ }; _ } ->
      Common.Logs.app (fun l -> l "No dependencies to pull, there's nothing to be done here!");
      Ok ()
  | { deps = { duniverse; _ }; _ } ->
      check_dune_lang_version ~yes ~repo >>= fun () ->
      let duniverse_dir = Fpath.(repo // Config.vendor_dir) in
      Bos.OS.Dir.create duniverse_dir >>= fun _created ->
      mark_duniverse_content_as_vendored ~duniverse_dir >>= fun () ->
      get_cache ~no_cache >>= fun cache -> pull_source_dependencies ~duniverse_dir ~cache duniverse

let no_cache =
  let doc = "Run without using the duniverse global cache" in
  Common.Arg.named (fun x -> `No_cache x) Cmdliner.Arg.(value & flag & info ~doc [ "no-cache" ])

let info =
  let open Cmdliner in
  let doc = "fetch the latest archives of the vendored libraries" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description;
      `P
        "This command reads the Git metadata calculated with $(i,duniverse lock) and fetches them \
         from their respective Git remotes and stores them in the $(b,duniverse/) directory in \
         the repository."
    ]
  in
  Term.info "pull" ~doc ~exits ~man

let term =
  Cmdliner.Term.(
    term_result (const run $ Common.Arg.yes $ no_cache $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
