{
open Astring

let line_ref = ref 1

let newline lexbuf =
  Lexing.new_line lexbuf;
  incr line_ref

let cram_header = Some "sh" (* FIXME: bash? *)
}

let eol = '\n' | eof
let ws = ' ' | '\t'

rule text section = parse
  | eof { [] }
  | ("#"+ as n) " " ([^'\n']* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: text (Some section) lexbuf }
  | "```" ([^' ' '\n']* as h) ws* ([^'\n']* as l) eol
      { let header = if h = "" then None else Some h in
        let contents = block lexbuf in
        let labels = Block.labels_of_string l in
        let value = Block.Raw in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        newline lexbuf;
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        newline lexbuf;
        `Block { Block.file; line; section; header; contents; labels; value }
        :: text section lexbuf }
  | ([^'\n']* as str) eol
      { newline lexbuf;
        `Text str :: text section lexbuf }

and block = parse
  | eof | "```" ws* eol    { [] }
  | ([^'\n'] * as str) eol { str :: block lexbuf }


and cram_text section = parse
  | eof { [] }
  | ("#"+ as n) " " ([^'\n']* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: cram_text (Some section) lexbuf }
  | "  " ([^'\n']* as first_line) eol
      { let header = cram_header in
        let contents = first_line :: cram_block lexbuf in
        let labels = [] in
        let value = Block.Raw in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        newline lexbuf;
        `Block { Block.file; line; section; header; contents; labels; value }
        :: cram_text section lexbuf }
  | "<-- non-deterministic" ws* (("command"|"output") as choice) eol
      { let header = cram_header in
        let contents = cram_block lexbuf in
        let labels = ["non-deterministic", Some (`Eq, choice)] in
        let value = Block.Raw in
        let file = lexbuf.Lexing.lex_start_p.Lexing.pos_fname in
        newline lexbuf;
        let line = !line_ref in
        List.iter (fun _ -> newline lexbuf) contents;
        newline lexbuf;
        `Block { Block.file; line; section; header; contents; labels; value }
        :: cram_text section lexbuf }
  | ([^'\n']* as str) eol
      { newline lexbuf;
        `Text str :: cram_text section lexbuf }

and cram_block = parse
  | eof | eol                   { [] }
  | "  " ([^'\n'] * as str) eol { str :: cram_block lexbuf }

{
type syntax = Normal | Cram

let token syntax lexbuf =
  try
    match syntax with
    | Normal -> text      None lexbuf
    | Cram   -> cram_text None lexbuf
  with Failure _ -> Misc.err lexbuf "incomplete code block"
}
