{
open Astring

type token = [ `Block of Block.Raw.t | `Section of int * string | `Text of string ]

let newline lexbuf = Lexing.new_line lexbuf
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
      "```" ([^' ' '\n']* as header) ws* ([^'\n']* as legacy_labels) eol
      { let contents = block lexbuf in
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
        let loc = Location.curr lexbuf in
        newline lexbuf;
        let block =
          Block.Raw.make ~loc ~section ~header ~contents ~label_cmt
            ~legacy_labels ~errors
        in
        (match errors with
         | [] -> ()
         | _ ->
           newline lexbuf;
           List.iter (fun _ -> newline lexbuf) errors;
           newline lexbuf);
        `Block block :: text section lexbuf }
  | "<!--" ws* "$MDX" ws* ([^' ' '\n']* as labels) ws* "-->" ws* eol
      { newline lexbuf;
        let loc = Location.curr lexbuf in
        let block = Block.Raw.make_include ~loc ~section ~labels in
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
      { let header = "sh" in
        let requires_empty_line, contents = cram_block lexbuf in
        let contents = first_line :: contents in
        let label_cmt = Some "" in
        let legacy_labels = "" in
        let loc = Location.curr lexbuf in
        List.iter (fun _ -> newline lexbuf) contents;
        let rest = cram_text section lexbuf in
        let block =
            Block.Raw.make ~loc ~section ~header ~contents ~label_cmt
              ~legacy_labels ~errors:[]
        in
        `Block block
        :: (if requires_empty_line then `Text "" :: rest else rest) }
  | "<-- non-deterministic" ws* ([^'\n']* as choice) eol
      { let header = "sh" in
        let requires_empty_line, contents = cram_block lexbuf in
        let label_cmt = Some (Printf.sprintf "non-deterministic=%s" choice) in
        let legacy_labels = "" in
        newline lexbuf;
        let loc = Location.curr lexbuf in
        List.iter (fun _ -> newline lexbuf) contents;
        let rest = cram_text section lexbuf in
        let block =
            Block.Raw.make ~loc ~section ~header ~contents ~label_cmt
              ~legacy_labels ~errors:[]
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
    | exn ->
      let loc = Location.curr lexbuf in
      let msg =
        Format.asprintf "%a: %s" Stable_printer.Location.print_loc loc (Printexc.to_string exn)
      in
      Util.Result.errorf "%s" msg


let cram_token lexbuf =
    try Ok (cram_text None lexbuf)
    with
    | exn ->
      let loc = Location.curr lexbuf in
      let msg =
        Format.asprintf "%a: %s" Stable_printer.Location.print_loc loc (Printexc.to_string exn)
      in
      Util.Result.errorf "%s" msg
}
