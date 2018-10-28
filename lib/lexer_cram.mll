{
open Astring
let commands s = String.cuts ~sep:"\\\n> " s
}

let eol = '\n' | eof
let ws = ' ' | '\t'
let digit = ['0' - '9']
let line = [^'\n']*

rule token = parse
 | eol                             { [] }
 | "[" (digit+ as str) "]" ws* eol { `Exit (int_of_string str) :: token lexbuf }
 | "..." ws* eol                   { `Ellipsis :: token lexbuf }
 | "$ " (line as str) '\\' eol     { `Command_first str :: token lexbuf }
 | "> " (line as str) '\\' eol     { `Command_cont  str :: token lexbuf }
 | "> " (line as str) eol          { `Command_last  str :: token lexbuf }
 | "$ " (line as str) eol          { `Command str :: token lexbuf }
 | (line as str) eol               { `Output str :: token lexbuf }

{
let token lexbuf =
  try token lexbuf
  with Failure _ -> Misc.err lexbuf "incomplete cram test"
}
