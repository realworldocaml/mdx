open Duniverse_lib
open Duniverse_lib.Types

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
  let root_packages =
    List.map Opam_cmd.split_opam_name_and_version root_packages |> Opam.sort_uniq
  in
  Ok { Duniverse.Config.root_packages; pull_mode; ocaml_compilers; opam_repo }

let compute_deps ~opam_entries =
  Dune_cmd.log_invalid_packages opam_entries;
  let get_default_branch remote = Exec.git_default_branch ~remote () in
  Duniverse.Deps.from_opam_entries ~get_default_branch opam_entries

let compute_depexts ~local_opam_repo pkgs =
  let open Rresult.R in
  Exec.map (Opam_cmd.get_opam_depexts ~local_opam_repo) pkgs >>| fun depexts ->
  List.flatten depexts |> List.sort_uniq Stdlib.compare

let resolve_ref deps =
  let resolve_ref ~upstream ~ref = Exec.git_resolve ~remote:upstream ~ref in
  Duniverse.Deps.resolve ~resolve_ref deps

let run (`Repo repo) 
    (`Opam_repo opam_repo) (`Pull_mode pull_mode) () =
  let open Rresult.R.Infix in
  (match Cloner.get_cache_dir () with None -> Ok (Fpath.v ".") | Some t -> t) >>= fun cache_dir ->
  let local_opam_repo = Fpath.(cache_dir / "opam-repository.git") in
  let opam_repo_url = Uri.with_fragment opam_repo None |> Uri.to_string in
  let opam_repo_branch = match Uri.fragment opam_repo with None -> "master" | Some b -> b in
  Exec.git_clone_or_pull ~remote:opam_repo_url ~branch:opam_repo_branch ~output_dir:local_opam_repo
  >>= fun () ->
  Opam_cmd.find_local_opam_packages repo >>= fun local_packages ->
  build_config ~local_packages ~pull_mode ~opam_repo
  >>= fun config ->
  Opam_cmd.calculate_opam ~config ~local_opam_repo >>= fun packages ->
  Opam_cmd.report_packages_stats packages;
  let depext_pkgs =
    config.root_packages @ List.map (fun { Types.Opam.package; _ } -> package) packages
  in
  compute_depexts ~local_opam_repo depext_pkgs
  >>= fun depexts ->
  Common.Logs.app (fun l ->
      l "Recording %a depext formulae for %a packages."
        Fmt.(styled `Green int)
        (List.length depexts)
        Fmt.(styled `Green int)
        (List.length depext_pkgs));
  List.iter (fun (k, v) -> Logs.info (fun l -> l "depext %s %s" (String.concat "," k) v)) depexts;
  Common.Logs.app (fun l -> l "Calculating Git repositories to vendor source code.");
  compute_deps ~opam_entries:packages >>= fun unresolved_deps ->
  resolve_ref unresolved_deps >>= fun deps ->
  let duniverse = { Duniverse.config; deps; depexts } in
  let file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.save ~file duniverse >>= fun () ->
  Common.Logs.app (fun l ->
      l "Wrote duniverse file with %a entries to %a. You can now run %a to fetch the sources."
        Fmt.(styled `Green int)
        (Duniverse.Deps.count duniverse.deps)
        Styled_pp.path (Fpath.normalize file)
        Fmt.(styled `Blue string)
        "duniverse pull");
  Ok ()

open Cmdliner

let opam_repo =
  let doc =
    "URL or path to the Duniverse opam-repository that has overrides for packages that have not \
     yet been ported to Dune upstream."
  in
  Common.Arg.named
    (fun x -> `Opam_repo (Uri.of_string x))
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
  Term.info "init" ~doc ~exits ~man ~envs:Common.Arg.caches

let term =
  let open Term in
  term_result
    ( const run $ Common.Arg.repo $ opam_repo $ pull_mode
    $ Common.Arg.setup_logs () )

let cmd = (term, info)
