open Duniverse_lib
open Duniverse_lib.Types
open Astring

let build_config ~local_packages ~pull_mode ~opam_repo =
  let open Rresult.R.Infix in
  Opam_cmd.choose_root_packages ~local_packages >>= fun root_packages ->
  let ocaml_compilers =
    match Dune_file.Project.supported_ocaml_compilers () with
    | Ok l -> List.map Ocaml_version.to_string l
    | Error (`Msg msg) ->
        Logs.warn (fun l -> l "%s" msg);
        []
  in
  let version = "1" in
  let root_packages =
    List.map Opam_cmd.split_opam_name_and_version root_packages |> Opam.sort_uniq
  in
  Ok { Duniverse.Config.version; root_packages; pull_mode; ocaml_compilers; opam_repo }

let compute_deps ~opam_entries =
  Dune_cmd.log_invalid_packages opam_entries;
  let get_default_branch remote = Exec.git_default_branch ~remote () in
  Duniverse.Deps.from_opam_entries ~get_default_branch opam_entries

let resolve_ref deps =
  let resolve_ref ~upstream ~ref = Exec.git_resolve ~remote:upstream ~ref in
  Duniverse.Deps.resolve ~resolve_ref deps

let init_local_opam_repo opam_repo =
  let open Rresult.R.Infix in
  (match Cloner.get_cache_dir () with
  | None -> Ok (Fpath.v ".")
  | Some t -> t) >>= fun cache_dir ->
  let local_opam_repo = Fpath.(cache_dir / "opam-repository.git") in
  let opam_repo_url = Uri.with_fragment opam_repo None |> Uri.to_string in
  let opam_repo_branch = match Uri.fragment opam_repo with None -> "master" | Some b -> b in
  Exec.git_clone_or_pull ~remote:opam_repo_url ~branch:opam_repo_branch ~output_dir:local_opam_repo >>= fun () ->
  Ok local_opam_repo

let run (`Repo repo)
    (`Opam_repo opam_repo) (`Pull_mode pull_mode) () =
  let open Rresult.R.Infix in
  init_local_opam_repo opam_repo >>= fun local_opam_repo ->
  Repo.local_packages repo >>= fun local_paths ->
  Repo.duniverse_file ~local_packages:local_paths repo >>= fun duniverse_file ->
  let local_packages = List.map fst (String.Map.bindings local_paths) in
  build_config ~local_packages ~pull_mode ~opam_repo
  >>= fun config ->
  let get_opam_path {Types.Opam.name; version} =
    match String.Map.find name local_paths with
    | Some path -> path
    | None ->
      let version = match version with None -> "dev" | Some v -> v in
      Fpath.(local_opam_repo / "packages" / name / (name ^ "." ^ version) / "opam")
  in
  let packages = config.root_packages in
  Opam_cmd.calculate_opam ~packages ~get_opam_path ~local_opam_repo >>= fun opam_entries ->
  Opam_cmd.report_packages_stats opam_entries;
  Common.Logs.app (fun l -> l "Calculating Git repositories to vendor source code.");
  compute_deps ~opam_entries >>= resolve_ref >>= fun deps ->
  let duniverse = { Duniverse.config; deps } in
  Duniverse.save ~file:duniverse_file duniverse >>= fun () ->
  Common.Logs.app (fun l ->
      l "Wrote duniverse file with %a entries to %a. You can now run %a to fetch the sources."
        Fmt.(styled `Green int)
        (Duniverse.Deps.count duniverse.deps)
        Styled_pp.path (Fpath.normalize duniverse_file)
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

let info =
  let exits = Term.default_exits in
  let doc = Fmt.strf "analyse opam files to generate an initial lock file" in
  let man = [] in
  Term.info "lock" ~doc ~exits ~man ~envs:Common.Arg.caches

let term =
  let open Term in
  term_result
    ( const run $ Common.Arg.repo $ Common.Arg.opam_repo $ pull_mode
    $ Common.Arg.setup_logs () )

let cmd = (term, info)
