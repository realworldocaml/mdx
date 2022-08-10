let sexp =
  let pp fmt s = Fmt.pf fmt "%s" (Mdx.Util.Csexp.to_string s) in
  Alcotest.testable pp ( = )

let msg =
  let pp fs = function `Msg s -> Fmt.pf fs "`Msg %S" s in
  Alcotest.testable pp ( = )

let ocaml_delimiter =
  let open Mdx.Ocaml_delimiter in
  let pp_part_meta pps { sep_indent; name } =
    Fmt.pf pps "{sep_indent=%s; name=%s}" sep_indent name
  in
  let pp pps = function
    | Part_begin part_meta -> Fmt.pf pps "Part_begin %a" pp_part_meta part_meta
    | Part_end -> Fmt.pf pps "Part_end"
    | Compat_attr part_meta ->
        Fmt.pf pps "Compat_attr %a" pp_part_meta part_meta
    | Content s -> Fmt.pf pps "Content %S" s
  in
  Alcotest.testable pp ( = )

let block = Alcotest.testable Mdx.Block.dump ( = )
let header = Alcotest.testable Mdx.Block.Header.pp ( = )
