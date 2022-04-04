open Import

let should_install ~yes pkgs =
  Prompt.confirm
    ~question:(fun l ->
      l
        "@[<hov 2>The following packages will be installed through %a:@ \
         %a@].@.Do you want to continue?"
        Fmt.(styled `Underline string)
        "sudo"
        Fmt.(list ~sep:(any "@ ") (styled `Bold string))
        pkgs)
    ~yes

let run (`Root root) (`Lockfile explicit_lockfile) dry_run (`Yes yes) () =
  let open Result.O in
  let* lockfile = Common.find_lockfile ~explicit_lockfile root in
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
      let pkgs_list = OpamSysPkg.Set.elements pkgs in
      match pkgs_list with
      | [] -> Ok ()
      | _ ->
          let pkgs_str = List.map ~f:OpamSysPkg.to_string pkgs_list in
          if dry_run then (
            Fmt.pr "%s\n%!" (String.concat ~sep:" " pkgs_str);
            Ok ())
          else if should_install ~yes pkgs_str then
            try
              OpamCoreConfig.update ~confirm_level:`unsafe_yes ();
              OpamSysInteract.install pkgs;
              Ok ()
            with Failure msg -> Error (`Msg msg)
          else Ok ())

open Cmdliner

let info =
  let exits = Cmd.Exit.defaults in
  let doc = Fmt.str "install external dependencies" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "This command installs the external dependencies listed in the \
         lockfile.";
    ]
  in
  Cmd.info "depext" ~doc ~exits ~man

let dry_run =
  let doc =
    Arg.info ~doc:"Display the system packages instead of installing them."
      [ "dry-run" ]
  in
  Arg.(value & flag doc)

let term =
  let open Term in
  term_result
    (const run $ Common.Arg.root $ Common.Arg.lockfile $ dry_run
   $ Common.Arg.yes $ Common.Arg.setup_logs ())

let cmd = Cmd.v info term
