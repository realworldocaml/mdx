(* This "program" is used to verify that the library is linkable - see #40 *)
let file = OpamParser.FullPos.file "../opam-file-format.opam"
in Printf.printf "Successfully loaded %s\n" file.OpamParserTypes.FullPos.file_name
