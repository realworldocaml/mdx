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

open Mdx.Util.Result.Infix

let src = Logs.Src.create "cram.pp"

module Log = (val Logs.src_log src : Logs.LOG)
module Util = Mdx.Util
module Block = Mdx.Block
module Toplevel = Mdx.Toplevel

let vpad_of_lines t =
  let rec aux i = function
    | h :: t when Mdx.Util.String.all_blank h -> aux (i + 1) t
    | _ -> i
  in
  aux 0 t

let pp_line_directive ppf (file, line) = Fmt.pf ppf "#%d %S" line file

let rec add_semi_semi = function
  | [] -> []
  | [ "" ] -> [ ";;" ]
  | x :: xs -> x :: add_semi_semi xs

let phrase_pad (phrase : Toplevel.t) = String.make (phrase.hpad + 2) ' '

let executable_contents (block : Block.t) =
  let loc = block.loc in
  let vpad = vpad_of_lines block.contents in
  let line_pairs =
    match block.value with
    | OCaml _ ->
        let block_start_loc = loc.loc_start.pos_lnum in
        let line = block_start_loc + vpad in
        [ (line, block.contents) ]
    | Toplevel _ ->
        Toplevel.of_lines ~loc block.contents
        (* TODO: use all values from the record *)
        |> (fun parsed -> parsed.tests)
        |> List.fold_left
             (fun acc (phrase : Toplevel.t) ->
               match phrase.command with
               | [] -> acc
               | commands ->
                   let mk s = Printf.sprintf "%s%s" (phrase_pad phrase) s in
                   let commands = List.map mk commands in
                   let commands = "" :: commands in
                   let line = phrase.pos.pos_lnum in
                   (line, commands) :: acc)
             []
        |> List.rev
    | Raw _ | Cram _ | Include _ -> []
  in
  line_pairs
  |> List.map (fun (line, contents) ->
         if contents = [] || Block.ends_by_semi_semi contents then
           (line, contents)
         else (line, add_semi_semi contents))

let run (`Setup ()) (`File file) (`Section section) =
  Mdx.parse_file Markdown file >>! fun t ->
  let t =
    match section with
    | None -> t
    | Some s -> (
        let re = Re.Perl.compile_pat s in
        match Mdx.filter_section re t with None -> [] | Some t -> t)
  in
  match t with
  | [] -> 1
  | t ->
      List.iter
        (function
          | Mdx.Section _ | Text _ -> ()
          | Block block when Mdx.Block.skip block -> ()
          | Block block ->
              Log.debug (fun l -> l "pp: %a" Mdx.Block.dump block);
              let pp_lines = Fmt.(list ~sep:(any "\n") string) in
              let contents = executable_contents block in
              List.iter
                (fun (line, contents) ->
                  Fmt.pr "%a%a\n" pp_line_directive (file, line) pp_lines
                    contents)
                contents)
        t;
      0

open Cmdliner

let term = Term.(const run $ Cli.setup $ Cli.file $ Cli.section)
let doc = "Pre-process markdown files to produce OCaml code."
let info = Cmd.info "pp" ~doc
let cmd = Cmd.v info term
