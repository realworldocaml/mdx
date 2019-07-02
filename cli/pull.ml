open Stdune
open Duniverse_lib

let pull_source_dependencies ~duniverse_dir src_deps =
  Exec.iter
    (fun { Duniverse.Deps.Source.dir; upstream; ref; _ } ->
      let output_dir = Fpath.(duniverse_dir / dir) in
      Common.Logs.app (fun l -> l "Pulling sources for %a." Styled_pp.path output_dir);
      Exec.git_archive ~output_dir ~remote:upstream ~tag:ref () )
    src_deps

let run repo () =
  let open Result.O in
  let duniverse_file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.load ~file:duniverse_file >>= function
  | { deps = { duniverse = []; _ }; _ } ->
      Common.Logs.app (fun l -> l "No dependencies to pull, there's nothing to be done here!");
      Ok ()
  | { deps = { duniverse; _ }; _ } ->
      let duniverse_dir = Fpath.(repo // Config.vendor_dir) in
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

let term = Cmdliner.Term.(term_result (const run $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
