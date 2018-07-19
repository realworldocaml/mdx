let eol = '\n' | eof
let ws = ' ' | '\t'

rule token = parse
 | eof           { [] }
 | "..." ws* eol { `Ellipsis :: token lexbuf }
 | "# "          { let c = phrase [] (Buffer.create 8) lexbuf in
                   `Command c :: token lexbuf }
 | ([^'#'] [^'\n']* as str) eol
                 { `Output  str :: token lexbuf }

and phrase acc buf = parse
  | "\n  \\\n  "
      { Lexing.new_line lexbuf;
        phrase ("" :: Buffer.contents buf :: acc) (Buffer.create 8) lexbuf }
  | "\n  "
      { Lexing.new_line lexbuf;
        phrase (Buffer.contents buf :: acc) (Buffer.create 8) lexbuf }
  | eol
      { Lexing.new_line lexbuf;
        List.rev (Buffer.contents buf :: acc) }
 | ";;" eol { List.rev ((Buffer.contents buf ^ ";;") :: acc) }
 | _ as c   { Buffer.add_char buf c; phrase acc buf lexbuf }

{
let token lexbuf =
  try token lexbuf
  with Failure _ -> Misc.err lexbuf "incomplete toplevel"
}
