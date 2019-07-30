module Fmt_ext = Fmt
open Stdune
open Duniverse_lib

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

let warn_about_head_commit ~ref ~commit () =
  Logs.info (fun l ->
      l "%a is not the HEAD commit for %a anymore" Styled_pp.commit commit Styled_pp.branch ref );
  Logs.info (fun l -> l "You might want to consider running 'duniverse update'");
  ()

let checkout_if_needed ~head_commit ~output_dir ~ref ~commit () =
  let open Result.O in
  if String.equal commit head_commit then Ok ()
  else (
    warn_about_head_commit ~ref ~commit ();
    Exec.git_unshallow ~repo:output_dir () >>= fun () -> Exec.git_checkout ~repo:output_dir commit )

let pull ~duniverse_dir src_dep =
  let open Result.O in
  let open Duniverse.Deps.Source in
  let { dir; upstream; ref = { Git.Ref.t = ref; commit }; _ } = src_dep in
  let output_dir = Fpath.(duniverse_dir / dir) in
  Common.Logs.app (fun l -> l "Pulling sources for %a." Styled_pp.path output_dir);
  Bos.OS.Dir.delete ~recurse:true output_dir >>= fun () ->
  Exec.git_shallow_clone ~output_dir ~remote:upstream ~ref () >>= fun () ->
  Exec.git_rev_parse ~repo:output_dir ~ref:"HEAD" () >>= fun head_commit ->
  checkout_if_needed ~head_commit ~output_dir ~ref ~commit ()
  |> Rresult.R.reword_error (fun (`Msg _) -> `Commit_is_gone dir)
  >>= fun () ->
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

let pull_source_dependencies ~duniverse_dir src_deps =
  let open Result.O in
  Parallel.map ~f:(pull ~duniverse_dir) src_deps
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

let run yes repo () =
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
      pull_source_dependencies ~duniverse_dir duniverse

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
    term_result (const run $ Common.Arg.yes $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
