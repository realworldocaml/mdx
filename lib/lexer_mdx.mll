{
open Result
open Astring

type token = [ `Block of Block.t | `Section of int * string | `Text of string ]

let newline lexbuf = Lexing.new_line lexbuf

exception Lex_error of Block_location.t * string

let labels ~loc l =
  match Label.of_string l with
  | Ok labels -> labels
  | Error msgs ->
    let msgs = List.map (fun (`Msg (x : string)) -> x) msgs in
    let msg = String.concat ~sep:" " msgs in
    raise (Lex_error (loc, msg))

let raise_error ~loc = function
  | Ok x -> x
  | Error (`Msg msg) -> raise (Lex_error (loc, msg))

let block_location lexbuf =
  let { Location.loc_start; _ } = Location.curr lexbuf in
  Block_location.of_lexpos loc_start
}

let eol = '\n' | eof
let ws = ' ' | '\t'

rule text section = parse
  | eof { [] }
  | ("#"+ as n) " " ([^'\n']* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: text (Some section) lexbuf }
  | ( "<!--" ws* "$MDX" ws* ([^' ' '\n']* as label_cmt) ws* "-->" ws* eol? )?
      "```" ([^' ' '\n']* as h) ws* ([^'\n']* as legacy_labels) eol
      { let loc = block_location lexbuf in
        let header = Block.Header.of_string h in
        let contents = block lexbuf in
        let labels, legacy_labels =
          match (label_cmt, legacy_labels) with
          | Some label_cmt, "" -> labels ~loc label_cmt, false
          | Some _, _ -> raise (Lex_error (loc, "cannot mix both block labels syntax"))
          | None, l -> labels ~loc l, true
        in
        let errors =
          match error_block lexbuf with
          | exception _ -> []
          | e ->
            List.map (fun x ->
                match String.trim x with
                | "..." -> `Ellipsis
                | _ -> `Output x) e
        in
        newline lexbuf;
        List.iter (fun _ -> newline lexbuf) contents;
        newline lexbuf;
        let block =
          Block.mk ~loc ~section ~header ~contents ~labels ~legacy_labels
            ~errors
          |> raise_error ~loc
        in
        (match errors with
         | [] -> ()
         | _ ->
           newline lexbuf;
           List.iter (fun _ -> newline lexbuf) errors;
           newline lexbuf);
        `Block block :: text section lexbuf }
  | "<!--" ws* "$MDX" ws* ([^' ' '\n']* as label_cmt) ws* "-->" ws* eol
      { let loc = block_location lexbuf in
        let labels = labels ~loc label_cmt in
        newline lexbuf;
        let block =
          Block.mk_include ~loc ~section ~labels
          |> raise_error ~loc
        in
        `Block block :: text section lexbuf }
  | ([^'\n']* as str) eol
      { newline lexbuf;
        `Text str :: text section lexbuf }

and block = parse
  | eof | "```" ws* eol    { [] }
  | ([^'\n'] * as str) eol { str :: block lexbuf }

and error_block = parse
  | "```mdx-error" ws* eol { block lexbuf }

and cram_text section = parse
  | eof { [] }
  | ("#"+ as n) " " ([^'\n']* as str) eol
      { let section = (String.length n, str) in
        newline lexbuf;
        `Section section :: cram_text (Some section) lexbuf }
  | "  " ([^'\n']* as first_line) eol
      { let loc = block_location lexbuf in
        let header = Some (Block.Header.Shell `Sh) in
        let requires_empty_line, contents = cram_block lexbuf in
        let contents = first_line :: contents in
        let labels = [] in
        let legacy_labels = false in
        List.iter (fun _ -> newline lexbuf) contents;
        let rest = cram_text section lexbuf in
        let block =
          Block.mk ~loc ~section ~header ~contents ~labels ~legacy_labels
            ~errors:[]
          |> raise_error ~loc
        in
        `Block block
        :: (if requires_empty_line then `Text "" :: rest else rest) }
  | "<-- non-deterministic" ws* ([^'\n']* as choice) eol
      { let loc = block_location lexbuf in
        let header = Some (Block.Header.Shell `Sh) in
        let requires_empty_line, contents = cram_block lexbuf in
        let label =
          Label.interpret "non-deterministic" (Some (Eq, choice))
          |> raise_error ~loc
        in
        let legacy_labels = false in
        newline lexbuf;
        List.iter (fun _ -> newline lexbuf) contents;
        let rest = cram_text section lexbuf in
        let block =
          Block.mk ~loc ~section ~header ~contents ~labels:[label]
            ~legacy_labels ~errors:[]
          |> raise_error ~loc
        in
        `Block block
        :: (if requires_empty_line then `Text "" :: rest else rest) }
  | ([^'\n']* as str) eol
      { newline lexbuf;
        `Text str :: cram_text section lexbuf }

and cram_block = parse
  | eof { false, [] }
  | eol { newline lexbuf; true, [] }
  | "  " ([^'\n'] * as str) eol
      { let requires_empty_line, lst = cram_block lexbuf in
        requires_empty_line, str :: lst }

{
let markdown_token lexbuf =
  try Ok (text None lexbuf)
  with
  | Lex_error (loc, msg) ->
    Util.Result.errorf "%a: invalid code block: %s" Block_location.pp loc msg

let cram_token lexbuf =
  try Ok (cram_text None lexbuf)
  with
  | Lex_error (loc, msg) ->
    Util.Result.errorf "%a: invalid code block: %s" Block_location.pp loc msg
}
