open Rresult
open Astring
module Cli_pull = Pull
open Duniverse_lib

let _update_dune_workspace () =
  (* TODO *)
  ()

let is_ocaml_compatible ?ocamlc config =
  let ovs = List.map Ocaml_version.of_string_exn config.Duniverse.Config.ocaml_compilers in
  let latest = List.rev ovs |> List.hd in
  (* TODO also need to check that compiler-libs are installed, as that doesnt happen in
  * many system packages. E.g. `apk add ocaml` also needs `ocaml-compiler-libs` *)
  match Exec.ocaml_version ?ocamlc () with
  | Error (`Msg msg) ->
      Logs.debug (fun l -> l "error from ocaml_version: %s, so need %a" msg Ocaml_version.pp latest);
       `Need latest
  | Ok ov ->
      if List.mem ov ovs then begin
         Logs.debug (fun l -> l "found compatible %a" Ocaml_version.pp ov);
        `Found ov
      end else begin
         Logs.debug (fun l -> l "incompatible %a so need %a" Ocaml_version.pp ov Ocaml_version.pp latest);
        `Need latest
      end
    
(* Determine what environment we are working in, and precisely which
 * tools need to be installed.
 * TODO: permit overriding this via the CLI to a specific strategy. *)
let probe_system config =
  (* This is the first probe function to run, so the environment is
   * unchanged. The environment may be mutated later, and then this
   * function will return a different result. *)
  let open R.Infix in
  let in_opam_env =
    match Sys.getenv_opt "OPAM_SWITCH_PREFIX" with
    | Some _ -> true
    | None -> false
  in
  Osrelease.Distro.v () >>= function
  | `Windows `Cygwin ->
     Logs.info (fun l -> l "Cygwin detected TODO");
     exit 1
  | `Windows _ ->
     Logs.info (fun l -> l "Windows detected (non Cygwin) TODO");
     exit 1
  | _ -> begin
    match in_opam_env with
    | true ->
       Logs.info (fun l -> l "Opam environment detected. Need to create a new local switch.");
       Ok `Opam_env
    | false -> begin
       match is_ocaml_compatible config with
       | `Found ov -> begin
          Logs.info (fun l -> l "Compatible system OCaml detected (%a)" Ocaml_version.pp ov);
          match Exec.dune_version () with
          | Error _ -> Ok (`System (`Ocaml true, `Dune false))
          | Ok _ -> Ok (`System (`Ocaml true, `Dune true))
       end
       | `Need _ -> begin
          Logs.info (fun l -> l "No opam, no compatible OCaml detected.");
          match Exec.dune_version () with
          | Error _ -> Ok (`System (`Ocaml false, `Dune false))
          | Ok _ -> Ok (`System (`Ocaml false, `Dune true))
       end
    end
  end

 let add_bootstrap_to_path repo =
    let path =
      match Bos.OS.Env.req_var "PATH" with
      | Ok path -> Fmt.strf "%a:%s" Fpath.pp Fpath.(repo // Config.bootstrap_dir / "bin") path
      | Error _ -> Fpath.(to_string (repo // Config.bootstrap_dir / "bin"))
    in
    Logs.info (fun l -> l "Setting PATH to %s" path);
    Bos.OS.Env.set_var "PATH" (Some path)
    (* TODO unset OCAMLDIR and CAML_LD_LIBRARY_PATH? opam sets these.
     * We are now using local switches  *)

let install_local_ocaml repo config cache =
   (* check for a local installed version first *)
   let local_ocamlc = Fpath.(repo // Config.bootstrap_dir / "bin" / "ocamlc") in
   match is_ocaml_compatible ~ocamlc:local_ocamlc config with
   | `Found ov ->
     Logs.info (fun l -> l "Using previously locally installed OCaml %a in %a" Ocaml_version.pp ov Fpath.pp local_ocamlc);
     Ok ()
   | `Need ov ->
     Logs.info (fun l -> l "Installing local copy of OCaml %a." Ocaml_version.pp ov);
     let output_dir = Config.ocaml_src_dir in
     let remote = "git://github.com/ocaml/ocaml" in
     let ref = Fmt.strf "%a" Ocaml_version.pp ov in
     Exec.git_resolve ~remote ~ref >>= fun {Git.Ref.commit; _} ->
     Cloner.clone_to ~output_dir ~remote ~ref ~commit cache >>= fun _ ->
     Exec.install_ocaml_to
       ~prefix:(Fpath.(repo // Config.bootstrap_dir)) 
       ~src:(Fpath.(repo // Config.ocaml_src_dir)) ()

let install_local_dune repo _config cache =
  Logs.info (fun l -> l "Installing local dune");
  let output_dir = Config.dune_src_dir in
  let remote = "git://github.com/ocaml/dune" in
  let ref = Config.dune_latest_tag in
  Exec.git_resolve ~remote ~ref >>= fun {Git.Ref.commit; _} ->
  Cloner.clone_to ~output_dir ~remote ~ref ~commit cache >>= fun _ ->
  Exec.install_dune_to
    ~prefix:(Fpath.(repo // Config.bootstrap_dir)) 
    ~src:(Fpath.(repo // Config.dune_src_dir)) () >>= fun () ->
  Ok ()

let gen_tools ~bootstrap_dir ~tools_repo targets config =
  let open Rresult.R.Infix in
  (* Generate a dune-project file in the tools dir with the right constraints *)
  let ofv name version =
    if List.mem (if name = "opam-client" then "opam" else name) targets then
    match version with
    | Duniverse.Tools.Latest  -> name
    | Duniverse.Tools.Eq ver -> Printf.sprintf "(%s (= %s))" name ver
    | Duniverse.Tools.Min ver -> Printf.sprintf "(%s (>= %s))" name ver
    else ""
  in
  let tools_opam =
    let { Duniverse.Tools.ocamlformat; opam; odoc; dune; mdx; lsp; merlin } = config.Duniverse.Config.tools in
    [ ofv "ocamlformat" ocamlformat;
      ofv "opam-client" opam;
      ofv "odoc" odoc;
      ofv "mdx" mdx;
      ofv "ocaml-lsp-server" lsp;
      ofv "merlin" merlin;
      ofv "dune" dune ] |> String.concat ~sep:" " in
  let dune_project = Printf.sprintf {|(lang dune 2.0)
(generate_opam_files true)
(name duniverse_tools)  
(package (name duniverse_tools)
(depends %s))
  |} tools_opam in
  Bos.OS.Dir.create tools_repo >>= fun _ ->
  Bos.OS.File.write (Fpath.(tools_repo / "dune-project")) dune_project >>= fun () ->
  let dune_workspace = Fmt.strf {|(lang dune 2.0)
(context (default (paths (PATH %a/bin :standard))))
(env (_ (flags -w -A))) |} Fpath.pp bootstrap_dir in
  Bos.OS.File.write (Fpath.(tools_repo / "dune-workspace")) dune_workspace

let install_local_tools repo opam_repo config cache strategy =
  (match strategy with
  | `Opam_env ->
    Common.Logs.app (fun l -> l "TODO create opam local switch as opam detected");
    exit 0
  | `System (`Ocaml ocaml_found, `Dune _dune_found) -> begin
      (if ocaml_found then begin
        Common.Logs.app (fun l -> l "Using system Ocaml.");
        Ok ()
      end else
        install_local_ocaml repo config.Duniverse.config cache)
      >>= fun () ->
      (Bos.OS.Cmd.exists (Bos.Cmd.v "dune") >>= function
       | true ->
          Common.Logs.app (fun l -> l "Using existing dune.");
          Ok ()
       | false ->
        install_local_dune repo config.Duniverse.config cache)
  end
  ) >>= fun () ->      
  (* Install tools *)
  let bootstrap_dir = Fpath.(repo // Config.bootstrap_dir) in
  Exec.iter (fun (tool_name, tool_targets) ->
    let tools_repo = Fpath.(repo // Config.tools_src_dir / tool_name) in
    gen_tools ~bootstrap_dir ~tools_repo tool_targets config.Duniverse.config >>= fun () ->  
    Exec.dune_build ~root:tools_repo ["duniverse_tools.opam"] >>= fun () ->
    Lock.run (`Repo tools_repo) (`Opam_repo opam_repo) (`Pull_mode Duniverse.Config.Source) () >>= fun () ->
    Cli_pull.run (`Yes true) (`No_cache false) (`Repo tools_repo) (`Duniverse_repos None) () >>= fun () ->
    let tools_vendor_dir = Fpath.(tools_repo // Config.vendor_dir) in
    Bos.OS.File.delete Fpath.(tools_vendor_dir / "dune") >>= fun _ ->
    (* TODO modify duniverse pull to add unvendored as a source mode *)
    Bos.OS.Dir.contents ~dotfiles:false ~rel:true tools_vendor_dir >>= fun dirs ->
    (* As a special case, opam is cloned as opam-client since there is no opam package *)
    let tool_dirs = List.map (function "opam" -> "opam-client" | v -> v) tool_targets in
    let targets = List.filter_map (fun dir ->
      Astring.String.cut ~sep:"." (Fpath.to_string dir) |> function
        | None -> None
        | Some (pkg, ver) when List.exists (fun x -> x = pkg) tool_dirs ->
            Some (Printf.sprintf "@duniverse/%s.%s/install" pkg ver)
        | Some _ -> None
        ) dirs in
    Common.Logs.app (fun l -> l "Building tools: %s" tool_name);
    Exec.dune_build ~profile:"release" ~root:tools_repo targets  >>= fun () ->
    Common.Logs.app (fun l -> l "Installing tools in %a" Fpath.pp bootstrap_dir);
    Exec.dune_install ~root:tools_repo ~prefix:bootstrap_dir ~sections:["man";"share";"bin";"doc"] tool_targets >>= fun () ->
    Ok ()
    ) Duniverse.Tools.tools


let tools (`Repo repo) (`Opam_repo opam_repo) (`No_cache no_cache) () =
  let open R.Infix in
  Repo.duniverse_file repo >>= fun duniverse_file ->
  Duniverse.load ~file:duniverse_file >>= fun config -> (* TODO better error handling if no dune-get file *)
  (* Check for OCaml *)
  Common.get_cache ~no_cache >>= fun cache ->
  probe_system config.Duniverse.config >>= fun strategy ->
  add_bootstrap_to_path repo >>= fun () ->
  install_local_tools repo opam_repo config cache strategy

(* CLI processing *)
open Cmdliner

let cmd =
  let term =
    let open Term in
    term_result
      ( const tools $ Common.Arg.repo $ Common.Arg.opam_repo $ Common.Arg.no_cache $ Common.Arg.setup_logs () ) in
  let info =
    let exits = Term.default_exits in
    let doc =
      "Make all the tools needed to build the project available"
    in
    let man = [] in
    Term.info "tools" ~doc ~exits ~man ~envs:Common.Arg.caches in
  (term, info)
