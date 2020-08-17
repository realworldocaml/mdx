open Duniverse_lib
open Rresult

let confirm_replace_pin name ~yes () =
  Prompt.confirm_or_abort ~yes ~question:(fun l ->
    l "A pin with name %a already exists, would you like to replace it?"
      Fmt.(styled `Yellow string) name)

let pin (`Pin_name pin_name) (`Pin_uri uri) (`Repo repo) (`Yes yes) () =
  Repo.duniverse_file repo >>= fun file ->
  Bos.OS.File.exists file >>= fun exists ->
  if not exists then
    Rresult.R.error_msgf
      "No %a file found, please run `duniverse init` before adding pins."
      Fpath.pp file
  else
    let tag = Uri.fragment uri in
    let uri = Uri.with_fragment uri None in
    let pin = { Types.Opam.pin = pin_name; url = Some (Uri.to_string uri); tag } in
    Duniverse.load ~file >>= fun duniverse ->
    begin
      if List.exists (fun pin -> pin.Types.Opam.pin = pin_name) duniverse.config.pins then
        confirm_replace_pin ~yes pin_name () >>= fun () ->
        Ok (List.filter (fun pin -> pin.Types.Opam.pin <> pin_name) duniverse.config.pins)
      else Ok duniverse.config.pins
    end >>= fun pins ->
    let config = { duniverse.config with pins = pin :: pins } in
    let duniverse = { duniverse with config } in
    Duniverse.save ~file duniverse >>= fun () ->
    Common.Logs.app (fun l ->
        l "Added pin %a to %a."
          Fmt.(styled `Yellow string) pin_name
          Styled_pp.path (Fpath.normalize file));
    Common.Logs.app (fun l -> l "Updating dependencies...");
    let opam_repo = duniverse.config.opam_repo in
    let pull_mode = duniverse.config.pull_mode in
    Init.run (`Repo repo) (`Opam_repo opam_repo) (`Pull_mode pull_mode) ()

let unpin (`Pin_name pin_name) (`Repo repo) () =
  Repo.duniverse_file repo >>= fun file ->
  Bos.OS.File.exists file >>= fun exists ->
  if not exists then
    Rresult.R.error_msgf
      "No %a file found, please run `duniverse init` before removing with pins."
      Fpath.pp file
  else
    Duniverse.load ~file >>= fun duniverse ->
    let pins_len = List.length duniverse.config.pins in
    let filtered = List.filter (fun pin -> pin.Types.Opam.pin <> pin_name) duniverse.config.pins in
    if pins_len = List.length filtered then
      R.error_msgf "Could not find pin `%s` in %a." pin_name Fpath.pp (Fpath.normalize file)
    else
      let config = { duniverse.config with pins = filtered } in
      let duniverse = { duniverse with config } in
      Duniverse.save ~file duniverse >>= fun () ->
      Bos.OS.File.delete Fpath.(Config.pins_dir / (pin_name ^ ".opam")) >>= fun () ->
      Common.Logs.app (fun l ->
          l "Removed pin %a from %a."
            Fmt.(styled `Yellow string) pin_name
            Styled_pp.path (Fpath.normalize file));
    Common.Logs.app (fun l -> l "Updating dependencies...");
    let opam_repo = duniverse.config.opam_repo in
    let pull_mode = duniverse.config.pull_mode in
    Init.run (`Repo repo) (`Opam_repo opam_repo) (`Pull_mode pull_mode) ()

open Cmdliner

let pin_name =
  let docv = "PACKAGE_NAME" in
  let doc = "The $(docv) to be added to the lock file." in
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
      ( const pin $ pin_name $ dev_repo $ Common.Arg.repo $ Common.Arg.yes $ Common.Arg.setup_logs () ) in
  let info =
    let exits = Term.default_exits in
    let doc =
      Fmt.strf "Add a pinned package dependency to the lock file"
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
    let doc = Fmt.strf "Remove a pinned package dependency from the lock file" in
    let man = [] in
    Term.info "unpin" ~doc ~exits ~man ~envs:Common.Arg.caches in
  (term, info)
