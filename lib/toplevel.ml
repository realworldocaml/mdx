(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "mdx"
module Log = (val Logs.src_log src : Logs.LOG)

open Astring
open Misc

type t = {
  line   : int;
  command: string list;
  output : Output.t list;
}

let dump_line ppf = function
  | #Output.t as o -> Output.dump ppf o
  | `Command c     -> Fmt.pf ppf "`Command %a" Fmt.(Dump.list dump_string) c

let command t = t.command
let output t = t.output

let dump ppf ({ line; command; output } : t) =
  Fmt.pf ppf "@[{line=%d;@ command=%a;@ output=%a}@]"
    line
    Fmt.(Dump.list dump_string) command
    Fmt.(Dump.list Output.dump) output

let pp_command ?(pad=0) ppf (t : t) = match t.command with
  | [] -> ()
  | l  ->
    let l = List.map (function "" -> "\\" | s  -> s) l in
    let sep ppf () = Fmt.pf ppf "\n%a  " pp_pad pad in
    Fmt.pf ppf "%a# %a\n" pp_pad pad Fmt.(list ~sep string) l

let pp ?pad ppf (t : t) =
  pp_command ?pad ppf t;
  pp_lines (Output.pp ?pad) ppf t.output

let lexbuf ~file ~line s =
  let lexbuf = Lexing.from_string s in
  let start =
    {lexbuf.Lexing.lex_start_p with pos_fname = file; pos_lnum = line}
  in
  let curr =
    {lexbuf.Lexing.lex_curr_p with pos_fname = file; pos_lnum = line}
  in
  lexbuf.lex_start_p <- start;
  lexbuf.lex_curr_p <- curr;
  lexbuf

let of_lines ~file ~line t =
  let pad = pad_of_lines t in
  let unpad line =
    if String.length line < pad then Fmt.failwith "invalide padding: %S" line
    else String.with_index_range line ~first:pad
  in
  let lines = List.map unpad t in
  let lines = String.concat ~sep:"\n" lines in
  let lines = Lexer_top.token (lexbuf ~file ~line lines) in
  Log.debug (fun l ->
      l "Toplevel.of_lines (pad=%d) %a" pad Fmt.(Dump.list dump_line) lines
    );
  let mk command line output = { command; line; output = List.rev output } in
  let rec aux command line output acc = function
    | []                  -> List.rev (mk command line output :: acc)
    | `Ellipsis as o :: t -> aux command line (o :: output) acc t
    | `Output _ as o :: t -> aux command line (o :: output) acc t
    | `Command cmd   :: t ->
      let line' = line + List.length command + List.length output in
      aux cmd line' [] (mk command line output :: acc) t
  in
  match lines with
  | `Command cmd :: t -> pad, aux cmd line [] [] t
  | _ -> Fmt.failwith "invalid toplevel block: %a" Fmt.(Dump.list string) t
