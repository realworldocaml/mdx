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

type section = int * string
type line = Section of section | Text of string | Block of Block.t
type t = line list

let pp_section ppf (level, text) =
  Fmt.pf ppf "%s %s" (String.make level '#') text

let dump_line ppf = function
  | Block block -> Fmt.pf ppf "Block %a" Block.dump block
  | Section section -> Fmt.pf ppf "Section %a" pp_section section
  | Text text -> Fmt.pf ppf "Text %S" text

let pp_line ?syntax ppf = function
  | Block block -> Block.pp ?syntax ppf block
  | Section section -> Fmt.pf ppf "%a\n" pp_section section
  | Text s -> Fmt.string ppf s

let dump ppf t = Fmt.Dump.(list dump_line) ppf t

let pp ?syntax ppf t =
  Fmt.pf ppf "%a\n" Fmt.(list ~sep:(any "\n") (pp_line ?syntax)) t

let to_string = Fmt.to_to_string pp

let envs t =
  List.fold_left
    (fun acc line ->
      match line with
      | Block b -> (
          match b.value with
          | OCaml { env; _ } | Toplevel { env; _ } -> Ocaml_env.Set.add env acc
          | Raw _ | Cram _ | Include _ -> acc)
      | Section _ | Text _ -> acc)
    Ocaml_env.Set.empty t
