let eol = '\n' | eof
let ws = ' ' | '\t'

rule token = parse
 | eof           { [] }
 | "..." ws* eol { `Ellipsis :: token lexbuf }
 | '\n'          { `Output "" :: token lexbuf }
 | "# "          { let c = phrase [] (Buffer.create 8) lexbuf in
                   `Command c :: token lexbuf }
 | ([^'#' '\n'] [^'\n']* as str) eol
                 { `Output str :: token lexbuf }

and phrase acc buf = parse
  | ("\n"* as nl) "\n  "
      { Lexing.new_line lexbuf;
        let nl = List.init (String.length nl) (fun _ -> "") in
        phrase (nl @ Buffer.contents buf :: acc) (Buffer.create 8) lexbuf }
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
