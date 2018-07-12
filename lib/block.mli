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

(** The type for block values. *)
type value = S.block_value

(** The type for supported code blocks. *)
type t = S.block

val dump: t Fmt.t

val pp_header: t Fmt.t
val pp_contents: t Fmt.t
val pp_footer: unit Fmt.t

val pp: t Fmt.t

val mode: t -> [`Non_det of [`Command|`Output] | `Normal]

val value: t -> value
val section: t -> (int * string) option
val header: t -> string option

val is_raw_ocaml: t -> bool
val executable_contents: t -> string list
val eval: t -> t
