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

type relation

val relation_compare : relation -> int -> int -> bool

type t =
  | Dir of string
  | Source_tree of string
  | File of string
  | Part of string
  | Env of string
  | Skip
  | Non_det of [`Output | `Command]
  | Version of relation * Ocaml_version.t
  | Require_package of string
  | Set of string * string
  | Unset of string

val pp : Format.formatter -> t -> unit

val of_string: string -> t list
(** [of_string s] cuts [s] into a list of labels.

    @raise a [Failure] exception when [s] does not represent valid labels. *)
