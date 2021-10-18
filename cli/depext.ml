open Import

let run (`Repo repo) (`Lockfile explicit_lockfile) dry_run (`Yes yes) () =
  let open Result.O in
  if yes then OpamCoreConfig.update ~confirm_level:`unsafe_yes ();
  Common.find_lockfile ~explicit_lockfile repo >>= fun lockfile ->
  let depexts = Lockfile.depexts lockfile in
  OpamGlobalState.with_ `Lock_none (fun global_state ->
      let env = OpamPackageVar.resolve_global global_state in
      let pkgs =
        List.fold_left
          ~f:(fun acc (pkgs, f) ->
            if OpamFilter.eval_to_bool ~default:true env f then
              OpamSysPkg.Set.union acc pkgs
            else acc)
          ~init:OpamSysPkg.Set.empty depexts
      in
      let pkgs, _ = OpamSysInteract.packages_status pkgs in
      let pkgl = OpamSysPkg.Set.elements pkgs in
      match pkgl with
      | [] -> ()
      | _ ->
          if dry_run then
            let pkgs = List.map ~f:OpamSysPkg.to_string pkgl in
            Fmt.pr "%s\n%!" (String.concat ~sep:" " pkgs)
          else OpamSysInteract.install pkgs);
  Ok ()

open Cmdliner

let info =
  let exits = Term.default_exits in
  let doc = Fmt.str "install external dependencies" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command installs the external dependencies listed in the \
         lockfile.";
    ]
  in
  Term.info "depext" ~doc ~exits ~man

let dry_run =
  let doc =
    Arg.info ~doc:"Display the system packages instead of installing them."
      [ "dry-run" ]
  in
  Arg.(value & flag doc)

let term =
  let open Term in
  term_result
    (const run $ Common.Arg.repo $ Common.Arg.lockfile $ dry_run
   $ Common.Arg.yes $ Common.Arg.setup_logs ())

let cmd = (term, info)
