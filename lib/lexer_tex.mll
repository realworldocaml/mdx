{
open Astring

type token = [ `Block of Block.Raw.t | `Section of int * string | `Text of string ]

let newline lexbuf = Lexing.new_line lexbuf

let loc ~start ~end_ =
  Location.{loc_start = start; loc_end = end_; loc_ghost = false}
}

let eol = '\n' | eof
let ws = ' ' | '\t'

rule text section = parse
  | eof { [] }
  | ("#"+ as n) " " ([^'\n']* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: text (Some section) lexbuf }
  | ( "%" ws* "$MDX" ws* ([^' ' '\n']* as label_cmt) ws* eol? )?
    "\begin{" ([^' ' '\n']* as header) ws* ([^'\n']* as legacy_labels) "}" eol
      { let start = Lexing.lexeme_start_p lexbuf in
        newline lexbuf;
        (match label_cmt with
         | Some _ -> newline lexbuf
         | None -> ());
        let contents = block lexbuf in
        (* we assume the multi-line block starts with an ""
           TODO: tie this to the regex match *)
        let contents = "" :: contents in
        let errors =
          match error_block lexbuf with
          | exception _ -> []
          | e ->
            List.map (fun x ->
                match String.trim x with
                | "..." -> `Ellipsis
                | _ -> `Output x) e
        in
        let end_ = Lexing.lexeme_start_p lexbuf in
        let loc = loc ~start ~end_ in
        let block =
          Block.Raw.make ~loc ~section ~header ~contents ~label_cmt
            ~legacy_labels ~errors
        in
        `Block block :: text section lexbuf }
  | "%" ws* "$MDX" ws* ([^' ' '\n']* as labels) ws* eol
      { let loc = Location.curr lexbuf in
        newline lexbuf;
        let block = Block.Raw.make_include ~loc ~section ~labels in
        `Block block :: text section lexbuf }
  | ([^'\n']* as str) eol
      { newline lexbuf;
        let str = String.append str "\n" in
        `Text str :: text section lexbuf }

and block = parse
  | eof | ws* as end_pad "\end" ws* eol
    { newline lexbuf;
      [end_pad] }
  | ([^'\n']* as str) eol
    { newline lexbuf;
      str :: block lexbuf }

and error_block = parse
  | "```mdx-error" ws* eol { newline lexbuf; block lexbuf }

and cram_block = parse
  | eof { false, [] }
  | eol { newline lexbuf; true, [] }
  | ("  " as ws) ([^'\n'] * as str) eol
      { let requires_empty_line, lst = cram_block lexbuf in
        newline lexbuf;
        requires_empty_line, (String.append ws str) :: lst }

{
  let latex_token lexbuf =
    try Ok (text None lexbuf)
    with
    | exn ->
      let loc = Location.curr lexbuf in
      let msg =
        Format.asprintf "%a: %s" Stable_printer.Location.pp loc
          (Printexc.to_string exn)
      in
      Util.Result.errorf "%s" msg
}
