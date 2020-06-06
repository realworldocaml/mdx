let read_opam file =
  let ic = open_in file in
  let t = OpamFile.OPAM.read_from_channel ic in
  let depexts = OpamFile.OPAM.depexts t in
  List.iter
    (fun (depexts, filter) ->
      Printf.printf "%s {%s}\n%!"
        (String.concat ", " depexts)
        (OpamFilter.to_string filter);
      let env v =
        prerr_endline (OpamVariable.Full.to_string v);
        None
      in
      let r = OpamFilter.eval_to_bool ~default:false env filter in
      Printf.printf "%b\n%!" r)
    depexts
