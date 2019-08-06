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

open Mdx.Compat

module Part = struct

  type t =
    { name: string;
      body: string; }

  let v ~name ~body = { name; body }
  let name {name;_} = name
  let body {body;_} = body

end

module Parse_parts =
struct

  let part_statement_re =
    let open Re in
    let ws = rep space in
    compile @@ whole_string @@ seq [
      ws; str "[@@@"; ws; str "part"; ws;
      str "\""; group (rep1 any); str "\"";
      ws; str "]"; ws; opt (str ";;"); ws;
    ]

  let make_part ~name ~lines =
    (* Remove empty lines at the end of the part *)
    let rec remove_empty = function
      | "" :: tl -> remove_empty tl
      | ls -> ls
    in
    let body = String.concat "\n" (List.rev (remove_empty lines)) in
    Part.v ~name ~body

  let rec parse_parts input name lines =
    match input_line input with
    | exception End_of_file -> [make_part ~name ~lines]
    | line ->
      match Re.exec_opt part_statement_re line with
      | None -> parse_parts input name (line :: lines)
      | Some groups ->
        let part = make_part ~name ~lines in
        let new_name = Re.Group.get groups 1 in
        part :: parse_parts input new_name []

  let of_file name =
    let input = open_in name in
    parse_parts input "" []

end

type file =
  | Parts of Part.t list
  | Body of (exn * string)

let read file =
  Parts (Parse_parts.of_file file)

let err_parse_error (e, _) =
  Fmt.failwith "Parse error: %a" Fmt.exn e

let find file ~part = match file, part with
  | Body (_, s), None      -> Some [s]
  | Body b, _ -> err_parse_error b
  | Parts parts, Some part ->
    (match List.find_opt (fun p -> String.equal (Part.name p) part) parts with
     | Some p -> Some [Part.body p]
     | None   -> None )
  | Parts parts, None      ->
    List.fold_left (fun acc p -> Part.body p :: [""] @ acc) [] parts
    |> List.rev
    |> fun x -> Some x

let rec replace_or_append part_name body = function
  | p :: tl when String.equal (Part.name p) part_name ->
    { p with body } :: tl
  | p :: tl ->
    p :: replace_or_append part_name body tl
  | [] ->
    [{ name = part_name; body }]

let replace file ~part ~lines = match file, part with
  | Body (e, _), None -> Body (e, String.concat "\n" lines)
  | Body b     , _    -> err_parse_error b
  | Parts parts, _    ->
    let part = match part with None -> "" | Some p -> p in
    let parts = replace_or_append part (String.concat "\n" lines) parts in
    Parts parts

let contents = function
  | Body (_, s) -> String.trim s ^ "\n"
  | Parts parts ->
    let lines =
      List.fold_left (fun acc p ->
          let body =  Part.body p in
          match Part.name p with
          | "" -> body :: acc
          | n  -> body :: ("\n[@@@part \"" ^ n ^ "\"] ;;\n") :: acc
        ) [] parts
    in
    let lines = List.rev lines in
    let lines = String.concat "\n" lines in
    String.trim lines ^ "\n"
