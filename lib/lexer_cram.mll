{
open Astring
let commands s = String.cuts ~sep:"\\\n> " s
}

let eol = '\n' | eof
let ws = ' ' | '\t'
let digit = ['0' - '9']

let cram_cmd = [^'\n' '\\']+ ("\\\n> " [^'\n' '\\'] +)*

rule token = parse
 | eol                             { [] }
 | "[" (digit+ as str) "]" ws* eol { `Exit (int_of_string str) :: token lexbuf }
 | "..." ws* eol                   { `Ellipsis :: token lexbuf }
 | "$ " (cram_cmd as str) eol      { `Command (commands str) :: token lexbuf }
 | ([^'\n']* as str) eol           { `Output str :: token lexbuf }

{
let token lexbuf =
  try token lexbuf
  with Failure _ -> Misc.err lexbuf "incomplete cram test"
}
