type t = { fname : string; line : int; column : int }

let none = { fname = "_none_"; line = 0; column = 0 }
let pp ppf t = Fmt.fmt "File %S, line %d, column %d" ppf t.fname t.line t.column

let of_lexpos { Lexing.pos_fname; pos_lnum; pos_bol; pos_cnum; _ } =
  { fname = pos_fname; line = pos_lnum; column = pos_cnum - pos_bol }

let to_lexpos ?(offset = 0) t =
  {
    Lexing.pos_fname = t.fname;
    pos_lnum = t.line;
    pos_bol = offset;
    pos_cnum = offset + t.column;
  }
