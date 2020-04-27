let sexp =
  let rec pp fmt s =
    match (s : Mdx.Util.Sexp.t) with
    | Atom s -> Fmt.pf fmt "Atom %S" s
    | List l ->
        let sep fmt () = Fmt.pf fmt "; " in
        Fmt.pf fmt "List [%a]" Fmt.(list ~sep pp) l
  in
  Alcotest.testable pp Mdx.Util.Sexp.equal

let msg =
  let pp fs = function `Msg s -> Fmt.pf fs "`Msg %S" s in
  Alcotest.testable pp ( = )

let ocaml_delimiter =
  let open Mdx.Ocaml_delimiter in
  let pp fs = function
    | Part_begin (src, { indent; payload }) ->
        Fmt.string fs "Part_begin";
        ( match src with
        | Cmt -> Fmt.string fs "Cmt"
        | Attr -> Fmt.string fs "Attr" );
        Fmt.fmt "indent:%s" fs indent;
        Fmt.fmt "payload:%s" fs payload
    | Part_end -> Fmt.string fs "Part_end"
  in
  Alcotest.testable pp ( = )
