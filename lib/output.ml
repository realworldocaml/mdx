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
open Compat

let ellipsis = "..."

(* From jbuilder's stdlib *)
let ansi_color_strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop i =
    if i = len then Buffer.contents buf
    else
      match str.[i] with
      | '\027' -> skip (i + 1)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  and skip i =
    if i = len then Buffer.contents buf
    else match str.[i] with 'm' -> loop (i + 1) | _ -> skip (i + 1)
  in
  loop 0

module Sub = struct
  type t = S of string | Ellipsis

  let pp ppf = function
    | S s -> Fmt.string ppf s
    | Ellipsis -> Fmt.string ppf ellipsis

  let dump ppf = function
    | S s -> Fmt.pf ppf "%S" s
    | Ellipsis -> Fmt.string ppf ellipsis
end

module Line = struct
  type t = Sub.t list

  let of_string s =
    let rec aux s =
      match Astring.String.cut ~sep:ellipsis s with
      | Some ("", "") -> [ Sub.Ellipsis ]
      | Some (x, "") -> [ Sub.S x; Sub.Ellipsis ]
      | Some ("", y) -> Sub.Ellipsis :: aux y
      | Some (x, y) -> Sub.S x :: Sub.Ellipsis :: aux y
      | None -> [ Sub.S s ]
    in
    aux Astring.(String.drop ~rev:true ~sat:Char.Ascii.is_blank s)

  let pp ?(pad = 0) ppf s =
    Fmt.pf ppf "%a%a\n" pp_pad pad (Fmt.list ~sep:Fmt.nop Sub.pp) s

  let dump ppf s = Fmt.pf ppf "[%a]" (Fmt.list Sub.dump) s

  let matches a ~ref:(b : t) =
    let to_re = function
      | Sub.S s -> Re.str s
      | Sub.Ellipsis -> Re.(group (rep any))
    in
    match (a, b) with
    | x, [ Sub.S y ] -> String.equal x y
    | _, [ Sub.Ellipsis ] -> true
    | _ ->
        let re =
          Re.(group @@ seq (bos :: List.rev (eos :: List.rev_map to_re b)))
        in
        Util.Option.is_some Re.(exec_opt (compile re) a)

  let ansi_color_strip line =
    List.map
      (function Sub.S l -> Sub.S (ansi_color_strip l) | Ellipsis -> Ellipsis)
      line
end

module Lines = struct
  type t = Line.t list

  let rec equal (a : string list) ~ref:(b : t) =
    match (a, b) with
    | [], [] -> true
    | [], x :: y -> (
        match x with
        | [] -> equal [] ~ref:y
        | Ellipsis :: x -> equal [] ~ref:(x :: y)
        | _ :: _ -> false )
    | _ :: _, [] -> false
    | a :: b, x :: y -> (
        match x with
        | [] -> equal (a :: b) ~ref:y
        | [ Ellipsis ] ->
            equal b ~ref:y || equal (a :: b) ~ref:y || equal b ~ref:(x :: y)
        | _ :: _ -> Line.matches a ~ref:x && equal b ~ref:y )

  let drop_until xs ~ref:x =
    let rec loop = function
      | v :: xs when not (Line.matches v ~ref:x) -> loop xs
      | xs -> xs
    in
    loop xs

  let merge (a : string list) (b : t) =
    let raw x = [ Sub.S (Astring.String.trim x) ] in
    let rec aux acc in_sync = function
      | a, [] -> List.rev_append acc (List.map raw a)
      | a, [ [ Sub.Ellipsis ] ] ->
          List.rev_append acc (List.map raw a @ [ [ Sub.Ellipsis ] ])
      | [], _ -> List.rev acc
      | xs, [ Ellipsis ] :: ([ Ellipsis ] :: _ as ys) -> aux acc in_sync (xs, ys)
      | xs, [ Ellipsis ] :: (y :: _ as ys) ->
          if in_sync then
            let rest = drop_until xs ~ref:y in
            if List.compare_length_with rest 0 = 0 then aux acc in_sync (xs, ys)
            else aux ([ Ellipsis ] :: acc) in_sync (rest, ys)
          else aux acc in_sync (xs, ys)
      | l :: xs, r :: ys ->
          if Line.matches l ~ref:r then
            aux (r :: acc) (Line.matches l ~ref:r) (xs, ys)
          else aux (raw l :: acc) (Line.matches l ~ref:r) (xs, ys)
    in
    aux [] true (a, b)
end
