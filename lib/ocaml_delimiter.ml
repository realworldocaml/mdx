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

open Result

type part_begin = { indent : string; payload : string }

type t = Part_begin of [ `Cmt | `Attr ] * part_begin | Part_end

module Regexp = struct
  let marker = Re.str "$MDX"

  let spaces = Re.rep1 Re.space

  let id = Re.(rep1 (alt [ alnum; char '_'; char '-' ]))

  let cmt_assign =
    let open Re in
    compile
    @@ seq
         [
           group (rep space);
           str "(*";
           spaces;
           marker;
           spaces;
           group id;
           str "=";
           group id;
           spaces;
           str "*)";
         ]

  let cmt_simple =
    let open Re in
    compile
    @@ seq [ str "(*"; spaces; marker; spaces; group id; spaces; str "*)" ]

  let attribute =
    let open Re in
    let ws = rep space in
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

let parse line =
  let error () =
    Util.Result.errorf "'%s' is not a valid ocaml delimiter for mdx" line
  in
  match Re.exec_opt Regexp.attribute line with
  | Some g -> (
      let indent = Re.Group.get g 1 in
      let name = Re.Group.get g 2 in
      let payload = Re.Group.get g 3 in
      match name with
      | "part" -> Ok (Some (Part_begin (`Attr, { indent; payload })))
      | _ -> error () )
  | None -> (
      match Re.exec_opt Regexp.cmt_assign line with
      | Some g -> (
          let indent = Re.Group.get g 1 in
          let name = Re.Group.get g 2 in
          let payload = Re.Group.get g 3 in
          match name with
          | "part-begin" -> Ok (Some (Part_begin (`Cmt, { indent; payload })))
          | _ -> error () )
      | None -> (
          match Re.exec_opt Regexp.cmt_simple line with
          | Some g -> (
              let name = Re.Group.get g 1 in
              match name with "part-end" -> Ok (Some Part_end) | _ -> error () )
          | None -> Ok None ) )
