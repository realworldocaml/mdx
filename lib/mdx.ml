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

let src = Logs.Src.create "ocaml-mdx"
module Lexer = Lexer
module Log = (val Logs.src_log src : Logs.LOG)

module Output = Output
module Cram = Cram
module Toplevel = Toplevel
module Library = Library
module Block = Block
module Migrate_ast = Migrate_ast
module Compat = Compat
module Util = Util
module Prelude = Prelude
module Syntax = Syntax

type line =
  | Section of (int * string)
  | Text    of string
  | Block   of Block.t

type t = line list

let dump_line ppf (l: line) = match l with
  | Block b        -> Fmt.pf ppf "Block %a" Block.dump b
  | Section (d, s) -> Fmt.pf ppf "Section (%d, %S)" d s
  | Text s         -> Fmt.pf ppf "Text %S" s

let dump = Fmt.Dump.list dump_line

let pp_line ?syntax ppf (l: line) = match l with
  | Block b        -> Fmt.pf ppf "%a\n" (Block.pp ?syntax) b
  | Section (d, s) -> Fmt.pf ppf "%s %s\n" (String.make d '#') s
  | Text s         ->
    match syntax with
    | Some Mli -> Fmt.pf ppf "%s" s
    | _ -> Fmt.pf ppf "%s\n" s

let pp ?syntax ppf t =
  Fmt.pf ppf "%a\n" Fmt.(list ~sep:(unit "\n") (pp_line ?syntax)) t

let to_string = Fmt.to_to_string pp

let section_of_line = function
  | Section s -> Some s
  | Text _    -> None
  | Block b   -> b.section

let filter_section re (t: t) =
  match
    List.filter (fun l -> match section_of_line l with
        | None        -> false
        | Some (_, s) -> Re.execp re s
      )  t
  with
  | [] -> None
  | l  -> Some l

let parse l =
  List.map (function
      | `Text t -> Text t
      | `Section s -> Section s
      | `Block b   -> Block b
    ) l

let parse_mli file_contents =
  (* Find the locations of the code blocks within [file_contents], then slice it up into
     [Text] and [Block] parts by using the starts and ends of those blocks as
     boundaries. *)
  let code_blocks = Mli_parser.docstring_code_blocks file_contents in
  let cursor = ref { Odoc_model.Location_.line = 1; column = 0 } in
  let tokens =
    List.map
      (fun (code_block : Mli_parser.Code_block.t) ->
         let pre_text =
           Text
             (Mli_parser.slice
                file_contents
                ~start:!cursor
                ~end_:code_block.location.start)
         in
         let block =
           Block
             { Block.line = code_block.location.start.line
             ; file = ""
             ; section = None
             ; labels = []
             ; header = Some "ocaml"
             ; contents = String.split_on_char '\n' code_block.contents
             ; value = Raw
             }
         in
         cursor := code_block.location.end_;
         [ pre_text; Text "{["; block; Text "]}" ])
      code_blocks
    |> List.concat
  in
  let eof =
    let lines = String.split_on_char '\n' file_contents in
    { Odoc_model.Location_.line = List.length lines
    ; column = String.length (List.rev lines |> List.hd)
    }
  in
  let eof_is_beyond_location (loc : Odoc_model.Location_.point) =
    eof.line > loc.line || (eof.line = loc.line && eof.column > loc.column)
  in
  if eof_is_beyond_location !cursor
  then (
    let remainder = Mli_parser.slice file_contents ~start:!cursor ~end_:eof in
    if not (String.equal remainder "") then tokens @ [ Text remainder ] else tokens)
  else tokens

type syntax = Syntax.t =
  | Normal
  | Cram
  | Mli

let parse_lexbuf file_contents syntax l =
  match syntax with
  | Syntax.Mli -> parse_mli file_contents
  | Syntax.Normal | Syntax.Cram -> parse (Lexer.token syntax l)

let parse_file syntax f = parse (Lexer.token syntax (snd (Misc.init f)))

let of_string syntax s =
  match syntax with
  | Syntax.Mli -> parse_mli s
  | Syntax.Normal | Syntax.Cram -> parse_lexbuf s syntax (Lexing.from_string s)

let eval syntax = function
  | Section _ | Text _ as x -> x
  | Block t as x ->
    let t' = Block.eval syntax t in
    if t == t' then x else Block t'

type expect_result =
  | Identical
  | Differs

let remove_whitespace str =
  List.fold_right (fun s acc ->
    String.cuts ~sep:s acc |> String.concat ~sep:""
  ) [ " "; "\n"; "\t" ] str

let run_str ~syntax ~f file =
  let file_contents, lexbuf = Misc.init file in
  let items = parse_lexbuf file_contents syntax lexbuf in
  let items = List.map (fun i -> eval syntax i) items in
  Log.debug (fun l -> l "run @[%a@]" dump items);
  let corrected = f file_contents items in
  (* The code blocks in mli syntax are often formatted by a tool like ocp-indent or
     ocamlformat, and it would be impossible to match this formatting exactly down to
     every space and newline.

     Instead, for this syntax only, we make the diff whitespace-invariant.
  *)
  let has_changed = match syntax with
    | Syntax.Cram | Syntax.Normal -> corrected <> file_contents
    | Syntax.Mli -> remove_whitespace corrected <> remove_whitespace file_contents
  in
  let result = if corrected <> file_contents then Differs else Identical in
  (result, corrected)

let write_file ~outfile content =
  let oc = open_out_bin outfile in
  output_string oc content;
  close_out oc

let run_to_stdout ?(syntax=Normal) ~f infile =
  let (_, corrected) = run_str ~syntax ~f infile in
  print_string corrected

let run_to_file ?(syntax=Normal) ~f ~outfile infile =
  let (_, corrected) = run_str ~syntax ~f infile in
  write_file ~outfile corrected

let run ?(syntax=Normal) ?(force_output=false) ~f infile =
  let outfile = infile ^ ".corrected" in
  let (test_result, corrected) = run_str ~syntax ~f infile in
  match force_output, test_result with
  | true, _
  | false, Differs ->
    write_file ~outfile corrected
  | false, Identical ->
    if Sys.file_exists outfile then Sys.remove outfile
