type t = { fname : string; line : int; column : int }

let none = { fname = ""; line = 0; column = 0 }
let pp ppf t =
  let fname = if t.fname = "" then "_none_" else t.fname in
  Fmt.fmt "File %S, line %d, column %d" ppf fname t.line t.column

let of_lexpos { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum; _ } =
  { fname = pos_fname; line = pos_lnum; column = pos_cnum - pos_bol }

let to_lexpos ?(offset = 0) t =
  {
    Lexing.pos_fname = t.fname;
    pos_lnum = t.line;
    pos_bol = offset;
    pos_cnum = offset + t.column;
  }
