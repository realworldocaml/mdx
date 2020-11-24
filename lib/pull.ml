open Import

let pull ?(trim_clone = false) ~global_state ~duniverse_dir src_dep =
  let open Result.O in
  let open Duniverse.Repo in
  let { dir; url; _ } = src_dep in
  let output_dir = Fpath.(duniverse_dir / dir) in
  Bos.OS.Dir.delete ~must_exist:false ~recurse:true output_dir >>= fun () ->
  let url = Url.to_opam_url url in
  Opam.pull_tree ~url ~dir:output_dir global_state >>= fun () ->
  if trim_clone then
    Bos.OS.Dir.delete ~must_exist:false ~recurse:true Fpath.(output_dir / ".git") >>= fun () ->
    Bos.OS.Dir.delete ~recurse:true Fpath.(output_dir // Config.vendor_dir)
  else Ok ()

let pull_source_dependencies ?trim_clone ~global_state ~duniverse_dir src_deps =
  let open Result.O in
  Result.List.iter ~f:(pull ?trim_clone ~global_state ~duniverse_dir) src_deps >>= fun () ->
  let total = List.length src_deps in
  let pp_count = Pp.Styled.good Fmt.int in
  Logs.app (fun l -> l "Successfully pulled %a/%a repositories" pp_count total pp_count total);
  Ok ()

let mark_duniverse_content_as_vendored ~duniverse_dir =
  let open Result.O in
  let dune_file = Fpath.(duniverse_dir / "dune") in
  let content = Dune_file.Raw.duniverse_dune_content in
  Logs.debug (fun l ->
      l "Writing %a:\n %s" Pp.Styled.path dune_file (String.concat ~sep:"\n" content));
  Persist.write_lines_hum dune_file content >>= fun () ->
  Logs.debug (fun l -> l "Successfully wrote %a" Pp.Styled.path dune_file);
  Ok ()

let duniverse ~repo ~global_state duniverse =
  if List.is_empty duniverse then Ok ()
  else
    let open Result.O in
    let duniverse_dir = Fpath.(repo // Config.vendor_dir) in
    Bos.OS.Dir.create duniverse_dir >>= fun _created ->
    mark_duniverse_content_as_vendored ~duniverse_dir >>= fun () ->
    pull_source_dependencies ~global_state ~trim_clone:true ~duniverse_dir duniverse
