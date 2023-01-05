(*
 * Copyright (c) 2019 Nathan Rebours <nathan.p.rebours@gmail.com>
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

module Result = struct
  module Infix = struct
    let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e
    let ( >>| ) r f = match r with Ok x -> Ok (f x) | Error _ as e -> e

    let ( >>! ) r f =
      match r with
      | Ok x -> f x
      | Error l ->
          List.iter
            (fun (`Msg m) -> Printf.eprintf "[mdx] Fatal error: %s\n" m)
            l;
          1

    let ( let* ) = ( >>= )
    let ( let+ ) = ( >>| )
  end

  let errorf fmt = Format.ksprintf (fun s -> Error (`Msg s)) fmt
  let to_error_list = function Ok x -> Ok x | Error err -> Error [ err ]

  module List = struct
    open Infix

    let fold ~f ~init l =
      let rec go acc = function
        | [] -> Ok acc
        | hd :: tl ->
            let* acc = f acc hd in
            go acc tl
      in
      go init l

    let map ~f l =
      fold
        ~f:(fun acc elm ->
          let+ elm' = f elm in
          elm' :: acc)
        ~init:[] l
      >>| List.rev

    let split l =
      let rec split_rec oks errors l =
        match l with
        | [] -> (List.rev oks, List.rev errors)
        | Ok x :: tl -> split_rec (x :: oks) errors tl
        | Error x :: tl -> split_rec oks (x :: errors) tl
      in
      split_rec [] [] l
  end
end

module File = struct
  let read_lines file =
    let ic = open_in file in
    let r = ref [] in
    try
      while true do
        r := input_line ic :: !r
      done;
      assert false
    with End_of_file ->
      close_in ic;
      List.rev !r
end

module Option = struct
  let is_some = function Some _ -> true | None -> false
  let value ~default = function Some v -> v | None -> default
end

module Sexp = struct
  type t = Atom of string | List of t list
end

module Csexp = Csexp.Make (Sexp)

module String = struct
  let english_concat ~last_sep words =
    let pf = Printf.sprintf in
    let rec aux acc = function
      | [] -> acc
      | [ last ] -> pf "%s %s %s" acc last_sep last
      | hd :: tl -> aux (pf "%s, %s" acc hd) tl
    in
    match words with
    | [] -> invalid_arg "Util.String.english_concat"
    | hd :: tl -> aux hd tl

  let english_conjonction words = english_concat ~last_sep:"and" words
  let all_blank = Astring.String.for_all Astring.Char.Ascii.is_white
end

module List = struct
  let find_map f l =
    let rec aux = function
      | [] -> None
      | h :: t -> ( match f h with Some x -> Some x | None -> aux t)
    in
    aux l

  let partition_until f xs =
    let rec loop = function
      | [] -> ([], [])
      | x :: xs -> (
          match f x with
          | true ->
              let trueish, falseish = loop xs in
              (x :: trueish, falseish)
          | false -> ([], x :: xs))
    in
    let trueish, falseish = loop xs in
    (List.rev trueish, falseish)
end

module Array = struct
  let slice t ~from ~to_ =
    let start_index, length = (from, to_ - from + 1) in
    Array.sub t start_index length
end

module Process = struct
  let rec waitpid_non_intr pid =
    try Unix.waitpid [] pid
    with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

  let wait ~pid =
    match snd (waitpid_non_intr pid) with WEXITED n -> n | _ -> 255
end

module Int = struct
  let min a b = if a < b then a else b
end

module Seq = struct
  (* [Seq.append] was added in 4.11, implement it for older versions *)
  let rec append seq1 seq2 () =
    match seq1 () with
    | Seq.Nil -> seq2 ()
    | Seq.Cons (x, next) -> Seq.Cons (x, append next seq2)
end
