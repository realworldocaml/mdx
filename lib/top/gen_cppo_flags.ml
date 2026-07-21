(* OxCaml compilers answer "true" to [ocamlopt -config-var ox] *)
let () =
  let ic = open_in_bin Sys.argv.(1) in
  let contents = really_input_string ic (in_channel_length ic) in
  close_in ic;
  if String.trim contents = "true" then print_string "-D\nOXCAML\n"
