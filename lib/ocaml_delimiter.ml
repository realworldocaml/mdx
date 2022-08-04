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

type syntax = Cmt | Attr
type part_begin = { indent : string; payload : string }
type t = Part_begin of syntax * part_begin | Part_end | Content of string

module Regexp = struct
  let marker = Re.str "$MDX"
  let spaces = Re.rep1 Re.space
  let id = Re.(rep1 (alt [ alnum; char '_'; char '-'; char '=' ]))
  let ws = Re.(rep space)

  let cmt =
    let open Re in
    compile
    @@ seq
         [
           group (non_greedy (rep any));
           group ws;
           str "(*";
           spaces;
           marker;
           spaces;
           group id;
           spaces;
           str "*)";
         ]

  let attribute =
    let open Re in
    compile @@ whole_string
    @@ seq
         [
           group ws;
           str "[@@@";
           ws;
           group id;
           ws;
           str "\"";
           group id;
           str "\"";
           ws;
           str "]";
           ws;
           opt (str ";;");
           ws;
         ]
end

let parse_attr line =
  match Re.exec_opt Regexp.attribute line with
  | Some g -> (
      let indent = Re.Group.get g 1 in
      let name = Re.Group.get g 2 in
      let payload = Re.Group.get g 3 in
      match name with
      | "part" -> [ Part_begin (Attr, { indent; payload }) ]
      | _ -> [])
  | None -> []

let parse_cmt line =
  match Re.exec_opt Regexp.cmt line with
  | Some g -> (
      let indent = Re.Group.get g 2 in
      match Re.Group.get g 3 with
      | "part-end" ->
          let entries =
            match Re.Group.get g 1 with
            | "" -> [ Part_end ]
            | s -> [ Content s; Part_end ]
          in
          Ok entries
      | s -> (
          match Astring.String.cut ~sep:"=" s with
          | Some ("part-begin", payload) ->
              Ok [ Part_begin (Cmt, { indent; payload }) ]
          | Some ("part-end", _) ->
              Util.Result.errorf
                "'part-end' delimiter does not accept a value. Please write \
                 '(* $MDX part-end *)' instead."
          | _ ->
              Util.Result.errorf "'%s' is not a valid ocaml delimiter for mdx."
                line))
  | None -> Ok []

let parse line =
  match parse_attr line with
  | [] -> parse_cmt line
  | delimiters -> Ok delimiters
