open Duniverse_lib
open Rresult


let pin (`Pin_name to_add) (`Pin_uri uri) (`Repo repo) () =
  let file = Fpath.(repo // Config.duniverse_file) in
  Bos.OS.File.exists file >>= fun exists ->
  if not exists then
    Rresult.R.error_msgf
      "No %a file found, please run `duniverse init` before adding pins."
      Fpath.pp file
  else
    let tag = Uri.fragment uri in
    let uri = Uri.with_fragment uri None in
    let pin = { Types.Opam.pin = to_add; url = Some (Uri.to_string uri); tag } in
    Duniverse.load ~file >>= fun duniverse ->
    if List.exists (fun pin -> pin.Types.Opam.pin = to_add) duniverse.config.pins then
      R.error_msgf "Could not add pin `%s` as this package already exists in %a."
        to_add Fpath.pp (Fpath.normalize file)
    else
    let config = { duniverse.config with pins = pin :: duniverse.config.pins } in
    let duniverse = { duniverse with config } in
    Duniverse.save ~file duniverse >>= fun () ->
    Common.Logs.app (fun l ->
        l "Added pin %a to %a. You can now run %a to update the dependencies."
          Fmt.(styled `Yellow string) to_add
          Styled_pp.path (Fpath.normalize file)
          Fmt.(styled `Blue string)
          "duniverse init");
    Ok ()


let unpin (`Pin_name to_remove) (`Repo repo) () =
  let file = Fpath.(repo // Config.duniverse_file) in
  Bos.OS.File.exists file >>= fun exists ->
  if not exists then
    Rresult.R.error_msgf
      "No %a file found, please run `duniverse init` before removing with pins."
      Fpath.pp file
  else
    Duniverse.load ~file >>= fun duniverse ->
    let pins_len = List.length duniverse.config.pins in
    let filtered = List.filter (fun pin -> pin.Types.Opam.pin <> to_remove) duniverse.config.pins in
    if pins_len = List.length filtered then
      R.error_msgf "Could not find pin `%s` in %a." to_remove Fpath.pp (Fpath.normalize file)
    else
      let config = { duniverse.config with pins = filtered } in
      let duniverse = { duniverse with config } in
      Duniverse.save ~file duniverse >>= fun () ->
      Bos.OS.File.delete Fpath.(Config.pins_dir / (to_remove ^ ".opam")) >>= fun () ->
      Common.Logs.app (fun l ->
          l "Removed pin %a from %a. You can now run %a to update the dependencies."
            Fmt.(styled `Yellow string) to_remove
            Styled_pp.path (Fpath.normalize file)
            Fmt.(styled `Blue string)
            "duniverse init");
      Ok ()


open Cmdliner

let pin_name =
  let docv = "PACKAGE_NAME" in
  let doc = "The $(docv) to be added to the " ^ Fpath.to_string Config.duniverse_file ^ " file." in
  Common.Arg.named
    (fun x -> `Pin_name x)
    Arg.(required & pos 0 (some string) None & info [] ~docv ~doc)

let dev_repo =
  let docv = "REPO_URI" in
  let doc = "The $(docv) used to pull the source code of the package to be pinned." in
  Common.Arg.named
    (fun x -> `Pin_uri x)
    Arg.(required & pos 1 (some Common.Arg.dev_repo) None & info [] ~docv ~doc)

let pin_cmd =
  let term =
    let open Term in
    term_result
      ( const pin $ pin_name $ dev_repo $ Common.Arg.repo $ Common.Arg.setup_logs () ) in
  let info =
    let exits = Term.default_exits in
    let doc =
      Fmt.strf "Add a pinned package dependency to $(b,%a)" Fpath.pp Config.duniverse_file
    in
    let man = [] in
    Term.info "pin" ~doc ~exits ~man ~envs:Common.Arg.caches in
  (term, info)

let unpin_cmd =
  let term =
    let open Term in
    term_result
      ( const unpin $ pin_name $ Common.Arg.repo $ Common.Arg.setup_logs () ) in
  let info =
    let exits = Term.default_exits in
    let doc =
      Fmt.strf "Remove a pinned package dependency from $(b,%a)" Fpath.pp Config.duniverse_file
    in
    let man = [] in
    Term.info "unpin" ~doc ~exits ~man ~envs:Common.Arg.caches in
  (term, info)

