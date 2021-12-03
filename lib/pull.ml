open Import

let pull ?(trim_clone = false) ~global_state ~duniverse_dir src_dep =
  let open Result.O in
  let open Duniverse.Repo in
  let { dir; url; hashes; _ } = src_dep in
  let output_dir = Fpath.(duniverse_dir / dir) in
  let url = Url.to_opam_url url in
  let open OpamProcess.Job.Op in
  Opam.pull_tree ~url ~hashes ~dir:output_dir global_state @@| fun result ->
  let* () = result in
  if trim_clone then
    let* () =
      Bos.OS.Dir.delete ~must_exist:false ~recurse:true
        Fpath.(output_dir / ".git")
    in
    Bos.OS.Dir.delete ~recurse:true Fpath.(output_dir // Config.vendor_dir)
  else Ok ()

let pull_source_dependencies ?trim_clone ~global_state ~duniverse_dir src_deps =
  let open Result.O in
  let jobs = !OpamStateConfig.r.dl_jobs in
  let* _ =
    OpamParallel.map ~jobs
      ~command:(pull ?trim_clone ~global_state ~duniverse_dir)
      src_deps
    |> Result.List.all
  in
  let total = List.length src_deps in
  let pp_count = Pp.Styled.good Fmt.int in
  Logs.app (fun l ->
      l "Successfully pulled %a/%a repositories" pp_count total pp_count total);
  Ok ()

let mark_duniverse_content_as_vendored ~duniverse_dir =
  let open Result.O in
  let dune_file = Fpath.(duniverse_dir / "dune") in
  let content = Dune_file.Raw.duniverse_dune_content in
  Logs.debug (fun l ->
      l "Writing %a:\n %s" Pp.Styled.path dune_file
        (String.concat ~sep:"\n" content));
  let* () = Persist.write_lines_hum dune_file content in
  Logs.debug (fun l -> l "Successfully wrote %a" Pp.Styled.path dune_file);
  Ok ()

let pre_pull_clean_up ~full ~duniverse_dir duniverse =
  if full then Bos.OS.Dir.delete ~must_exist:false ~recurse:true duniverse_dir
  else
    Result.List.iter duniverse ~f:(fun { Duniverse.Repo.dir; _ } ->
        Bos.OS.Dir.delete ~must_exist:false ~recurse:true
          Fpath.(duniverse_dir / dir))

let duniverse ~full ~root ~global_state ~trim_clone duniverse =
  if List.is_empty duniverse then Ok ()
  else
    let open Result.O in
    let duniverse_dir = Fpath.(root // Config.vendor_dir) in
    let* () = pre_pull_clean_up ~full ~duniverse_dir duniverse in
    let* _created = Bos.OS.Dir.create duniverse_dir in
    let* () = mark_duniverse_content_as_vendored ~duniverse_dir in
    pull_source_dependencies ~global_state ~trim_clone ~duniverse_dir duniverse
