open Stdune
open Duniverse_lib

let show_opam_depexts config env =
  List.fold_left config.Duniverse.depexts ~init:[]
    ~f:(fun acc (tags, filter) ->
      if OpamFilter.eval_to_bool ~default:false env filter then tags @ acc else acc)
  |> fun tags -> List.sort_uniq ~compare:String.compare tags |> List.iter ~f:print_endline

let run (`Repo repo) () =
  let open Rresult.R in
  Repo.duniverse_file repo >>= fun duniverse_file ->
  Duniverse.load ~file:duniverse_file >>= fun config ->
  Osrelease.Distro.v () >>= fun distro ->
  Osrelease.Version.v () >>= fun version ->
  Osrelease.Arch.v () |> fun arch ->
  Osrelease.OS.v () |> fun os ->
  let env var =
    let return s = Some (OpamVariable.string s) in
    match OpamVariable.Full.variable var |> OpamVariable.to_string with
    | "os" -> return (Osrelease.OS.to_string os)
    | "os-distribution" -> return (Osrelease.Distro.to_string distro)
    | "os-version" -> ( match version with Some v -> return v | None -> None )
    | "os-family" -> return (Osrelease.Distro.to_string distro)
    | "arch" -> return (Osrelease.Arch.to_string arch)
    | _ -> None
  in
  show_opam_depexts config env;
  Ok ()

let info =
  let open Cmdliner in
  let doc = "print external packages required to build this duniverse" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command prints the external operating system dependencies used by the local \
         duniverse. The results can be piped to your system manager\n\
        \          to install the packages needed for a successful dune build.";
    ]
  in
  Term.info "depext" ~doc ~exits ~man

let term = Cmdliner.Term.(term_result (const run $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)
