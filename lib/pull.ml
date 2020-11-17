open Import

let report_commit_is_gone_repos repos =
  let sep fmt () =
    Format.pp_print_newline fmt ();
    Pp.Styled.header_indent fmt ();
    Fmt.(const string "  - ") fmt ()
  in
  let fmt_repos = Fmt.(list ~sep Pp.Styled.package_name) in
  Logs.app (fun l ->
      l "The following repos could not be pulled as the commit we want is gone:%a%a" sep ()
        fmt_repos repos);
  Logs.app (fun l ->
      l "You should run 'duniverse update' to fix the commits associated with the tracked refs")

let pull ?(trim_clone = false) ~duniverse_dir ~cache src_dep =
  let open Result.O in
  let open Duniverse.Deps.Source in
  let { dir; upstream; ref = { Git.Ref.t = ref; commit }; _ } = src_dep in
  let output_dir = Fpath.(duniverse_dir / dir) in
  Bos.OS.Dir.delete ~must_exist:false ~recurse:true output_dir >>= fun () ->
  Cloner.clone_to ~output_dir ~remote:upstream ~ref ~commit cache
  |> Rresult.R.reword_error (fun (`Msg _) -> `Commit_is_gone dir)
  >>= fun _cached ->
  (* Common.Logs.app (fun l ->
      l "Pulled sources for %a.%a" Pp.Styled.path output_dir Pp.Styled.cached cached); *)
  if trim_clone then
    Bos.OS.Dir.delete ~must_exist:true ~recurse:true Fpath.(output_dir / ".git") >>= fun () ->
    Bos.OS.Dir.delete ~recurse:true Fpath.(output_dir // Config.vendor_dir)
  else Ok ()

let pull_source_dependencies ?trim_clone ~duniverse_dir ~cache src_deps =
  let open Result.O in
  List.map ~f:(pull ?trim_clone ~duniverse_dir ~cache) src_deps
  |> Result.List.fold_left ~init:[] ~f:(fun acc res ->
         match res with
         | Ok () -> Ok acc
         | Error (`Commit_is_gone dir) -> Ok (dir :: acc)
         | Error (`Msg _ as err) -> Error (err :> [> `Msg of string ]))
  >>= function
  | [] ->
      let total = List.length src_deps in
      let pp_count = Pp.Styled.good Fmt.int in
      Logs.app (fun l -> l "Successfully pulled %a/%a repositories" pp_count total pp_count total);
      Ok ()
  | commit_is_gone_repos ->
      report_commit_is_gone_repos commit_is_gone_repos;
      Error (`Msg "Could not pull all the source dependencies")

let mark_duniverse_content_as_vendored ~duniverse_dir =
  let open Result.O in
  let dune_file = Fpath.(duniverse_dir / "dune") in
  let content = Dune_file.Raw.duniverse_dune_content in
  Logs.debug (fun l ->
      l "Writing %a:\n %s" Pp.Styled.path dune_file (String.concat ~sep:"\n" content));
  Persist.write_lines_hum dune_file content >>= fun () ->
  Logs.debug (fun l -> l "Successfully wrote %a" Pp.Styled.path dune_file);
  Ok ()

let submodule_add ~repo ~duniverse_dir src_dep =
  let open Result.O in
  let open Duniverse.Deps.Source in
  let { dir; upstream; ref = { Git.Ref.t = _ref; commit }; _ } = src_dep in
  let remote_name = match Astring.String.cut ~sep:"." dir with Some (p, _) -> p | None -> dir in
  let target_path = Fpath.(normalize (duniverse_dir / dir)) in
  let frag =
    Printf.sprintf "[submodule %S]\n  path=%s\n  url=%s" remote_name (Fpath.to_string target_path)
      upstream
  in
  let cacheinfo = (160000, commit, target_path) in
  Exec.git_update_index ~repo ~add:true ~cacheinfo () >>= fun () ->
  (* Common.Logs.app (fun l -> l "Added submodule for %s." dir); *)
  Ok frag

let set_git_submodules ~repo ~duniverse_dir src_deps =
  let open Result.O in
  List.map ~f:(submodule_add ~repo ~duniverse_dir) src_deps
  |> Result.List.fold_left ~init:[] ~f:(fun acc res ->
         match res with
         | Ok frag -> Ok (frag :: acc)
         | Error (`Msg _ as err) -> Error (err :> [> `Msg of string ]))
  >>= fun git_sm_frags ->
  let git_sm = String.concat ~sep:"\n" git_sm_frags in
  Bos.OS.File.write Fpath.(repo / ".gitmodules") git_sm >>= fun () ->
  (* Common.Logs.app (fun l -> l "Successfully wrote gitmodules."); *)
  Ok ()

let duniverse ~cache ~pull_mode ~repo duniverse =
  if List.is_empty duniverse then Ok ()
  else
    let open Result.O in
    let duniverse_dir = Fpath.(repo // Config.vendor_dir) in
    Bos.OS.Dir.create duniverse_dir >>= fun _created ->
    mark_duniverse_content_as_vendored ~duniverse_dir >>= fun () ->
    let sm = pull_mode = Duniverse.Config.Submodules in
    pull_source_dependencies ~trim_clone:(not sm) ~duniverse_dir ~cache duniverse >>= fun () ->
    if sm then set_git_submodules ~repo ~duniverse_dir duniverse else Ok ()
