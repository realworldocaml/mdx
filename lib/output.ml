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

open Misc

type t = [`Output of string | `Ellipsis]

let dump ppf = function
  | `Output s -> Fmt.pf ppf "`Output %S" s
  | `Ellipsis -> Fmt.pf ppf "`Ellipsis"

let pp ?(pad=0) ppf = function
  | `Output s -> Fmt.pf ppf "%a%s\n" pp_pad pad s
  | `Ellipsis -> Fmt.pf ppf "%a...\n" pp_pad pad

let equal a b =
  let rec aux x y = match x, y with
    | [], []  | [`Ellipsis], _   | _, [`Ellipsis]  -> true
    | (`Ellipsis::a as x), (_::b as y) | (_::b as y), (`Ellipsis::a as x) ->
      aux x b || (* n+ matches: skip y's head *)
      aux a y    (* 0  match  : skip x's head *)
    | a::b, h::t -> a = h && aux b t
    | _ -> false
  in
  aux a b
