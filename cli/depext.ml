open Stdune
open Duniverse_lib

let show_opam_depexts config env =
  (* TODO cant find a OpamFilter.of_string, so parse a minimal opam buffer
     to reconstruct formula instead *)
  let b = Buffer.create 1024 in
  Buffer.add_string b "opam-version: \"2.0\"\ndepexts: [\n";
  List.iter ~f:(fun (s,f) -> 
    let tags = String.concat ~sep:" " (List.map ~f:(Printf.sprintf "%S") s) in
    Printf.bprintf b "  [ %s ] {%s}\n" tags f;
  ) config.Duniverse.depexts;
  Buffer.add_string b "]\n";
  Buffer.to_bytes b |> Bytes.to_string |>
  OpamFile.OPAM.read_from_string |>
  OpamFile.OPAM.depexts |>
  List.fold_left ~init:[] ~f:(fun acc (tags, filter) ->
    if OpamFilter.eval_to_bool ~default:false env filter then
      tags @ acc else acc
  ) |> fun tags ->
  List.iter ~f:print_endline tags

let run (`Repo repo) () =
  let open Rresult.R in
  let duniverse_file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.load ~file:duniverse_file >>= fun config ->
  Osrelease.Distro.v () >>= fun distro ->
  Osrelease.Version.v () >>= fun version ->
  Osrelease.Arch.v () |> fun arch ->
  Osrelease.OS.v () |> fun os ->
  let env = fun var ->
    let return s = Some (OpamVariable.string s) in
    match OpamVariable.Full.variable var |> OpamVariable.to_string with
    |"os" -> return (Osrelease.OS.to_opam_string os)
    |"os-distribution" -> return (Osrelease.Distro.to_opam_string distro)
    |"os-version" -> (match version with Some v -> return v | None -> None)
    |"os-family" -> return (Osrelease.Distro.to_opam_string distro)
    |"arch" -> return (Osrelease.Arch.to_opam_string arch)
    |_ -> None
  in
  Common.Logs.app (fun l -> l "OS detected as %s with version %s" 
    (Osrelease.Distro.to_opam_string distro) 
    (match version with None -> "?" | Some v -> v)); 
  show_opam_depexts config env;
  Ok ()

let info =
  let open Cmdliner in
  let doc = "print external packages required to build this duniverse" in
  let exits = Term.default_exits in
  let man =
    [
      `S Manpage.s_description;
      `P "This command prints the external operating system dependencies used \
          by the local duniverse. The results can be piped to your system manager
          to install the packages needed for a successful dune build.";
    ]
  in
  Term.info "depext" ~doc ~exits ~man 

let term =
  Cmdliner.Term.(term_result (const run $ Common.Arg.repo $ Common.Arg.setup_logs ()))

let cmd = (term, info)


