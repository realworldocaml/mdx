(* OxCaml compilers report a version of the form X.Y.Z+ox *)
let () =
  let version = Sys.argv.(1) in
  let is_oxcaml =
    (* When min supported version is ocaml>=4.13 we can use String.ends_with *)
    match String.index_opt version '+' with
    | Some i ->
        let build = String.sub version (i + 1) (String.length version - i - 1) in
        String.length build >= 2 && String.sub build 0 2 = "ox"
    | None -> false
  in
  if is_oxcaml then print_string "-D\nOXCAML\n"
