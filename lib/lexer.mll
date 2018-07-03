{

open Astring
open S

let pp_position ppf lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  Fmt.pf ppf
    "File \"%s\", line %d, character %d"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol)

(* TODO: better error reporting *)
let err lexbuf fmt =
  Fmt.kstrf (fun str ->
      Fmt.failwith "%a: %s" pp_position lexbuf str
    ) fmt

let commands s = String.cuts ~sep:"\\\n  > " s
let line_ref = ref 1

}

let eol = '\n' | eof
let ws = ' ' | '\t'
let cmd = [^'\n' '\\']+ ("\\\n  > " [^'\n' '\\']+)*
let digit = ['0' - '9']

rule text section = parse
  | eof { [] }
  | ("#"+ as n) " " ([^'\n']* as str) eol
      { let section = (String.length n, str) in
        incr line_ref;
        Section section :: text (Some section) lexbuf }
  | "```" ([^' ' '\n']* as h) ws* ([^'\n']* as l) eol
      { let header = if h = "" then None else Some h in
        let contents = block lexbuf in
        let labels = String.cuts ~empty:false ~sep:"," l in
        let value = Raw in
        incr line_ref;
        let line = !line_ref in
        List.iter (fun _ -> incr line_ref) contents;
        incr line_ref;
        Block { line; section; header; contents; labels; value }
        :: text section lexbuf }
  | ([^'\n']* as str) eol
      { incr line_ref;
        Text str :: text section lexbuf }

and block = parse
  | eol | "```" ws* eol    { [] }
  | ([^'\n'] * as str) eol { str :: block lexbuf }

and cram = parse
 | "✘ exit" ws+ (digit + as str) ws* eol { `Exit (int_of_string str) }
 | "..." ws* eol                         { `Ellipsis }
 | "$ " (cmd as str) eol                 { `Command (commands str) }
 | ([^'\n']* as str) eol                 { `Output  str }

and toplevel = parse
 | "..." ws* eol                         { `Ellipsis }
 | "# " (cmd as str) eol                 { `Command (commands str) }
 | ([^'\n']* as str) eol                 { `Output  str }

{

let token lexbuf = text None lexbuf

}
