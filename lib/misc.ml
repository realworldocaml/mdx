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

open Astring

let rec hpad_of_lines = function
  | [] -> 0
  | h :: hs -> (
      match Util.String.all_blank h with
      | true -> hpad_of_lines hs
      | false ->
          let i = ref 0 in
          while !i < String.length h && h.[!i] = ' ' do
            incr i
          done;
          !i)

let pp_pad ppf = function
  | 0 -> ()
  | i -> Fmt.string ppf (String.v ~len:i (fun _ -> ' '))

let pp_lines pp = Fmt.(list ~sep:(any "\n") pp)

let read_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let file_contents = really_input_string ic len in
  close_in ic;
  file_contents

type loaded_file = { lexbuf : Lexing.lexbuf; string : string }

let load_file ~filename =
  let file_contents = read_file filename in
  let lexbuf = Lexing.from_string file_contents in
  lexbuf.lex_curr_p <-
    { pos_fname = filename; pos_cnum = 0; pos_lnum = 1; pos_bol = 0 };
  { string = file_contents; lexbuf }

let pp_position ppf lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  Fmt.pf ppf "File \"%s\", line %d, character %d" p.Lexing.pos_fname
    p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol)

(* TODO: better error reporting *)
let err lexbuf fmt =
  Fmt.kstr (fun str -> Fmt.failwith "%a: %s" pp_position lexbuf str) fmt
