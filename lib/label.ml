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

open Compat
open Result

module Relation = struct
  type t = Eq | Neq | Le | Lt | Ge | Gt

  let pp ppf = function
    | Eq -> Fmt.string ppf "="
    | Neq -> Fmt.string ppf "<>"
    | Gt -> Fmt.string ppf ">"
    | Ge -> Fmt.string ppf ">="
    | Lt -> Fmt.string ppf "<"
    | Le -> Fmt.string ppf "<="

  let compare = function
    | Eq -> ( = )
    | Neq -> ( <> )
    | Lt -> ( < )
    | Le -> ( <= )
    | Gt -> ( > )
    | Ge -> ( >= )

  let raw_parse_1_char op op_char s =
    match String.index_opt s op_char with
    | Some i ->
      let before = String.sub s 0 i in
      let after = String.sub s (i+1) (String.length s - i - 1) in
      (before, Some (op, after))
    | None -> (s, None)

  let raw_parse_2_chars op op_char_1 op_char_2 s =
    match String.index_opt s op_char_1 with
    | Some i
      when String.length s >= i + 2
        && Char.equal (String.get s (i+1)) op_char_2 ->
      let before = String.sub s 0 i in
      let after = String.sub s (i+2) (String.length s - i - 2) in
      (before, Some (op, after))
    | _ -> (s, None)

  let ( ||| ) x y =
    match x with
    | _, None -> y
    | x -> x

  let raw_parse s =
    raw_parse_2_chars Neq '<' '>' s |||
    raw_parse_2_chars Ge '>' '=' s |||
    raw_parse_1_char Gt '>' s |||
    raw_parse_2_chars Le '<' '=' s |||
    raw_parse_1_char Lt '<' s |||
    raw_parse_1_char Eq '=' s
end

type t =
  | Dir of string
  | Source_tree of string
  | File of string
  | Part of string
  | Env of string
  | Skip
  | Non_det of [`Output | `Command]
  | Version of Relation.t * Ocaml_version.t
  | Require_package of string
  | Set of string * string
  | Unset of string

let pp ppf = function
  | Dir d -> Fmt.pf ppf "dir=%s" d
  | Source_tree s -> Fmt.pf ppf "source-tree=%s" s
  | File f -> Fmt.pf ppf "file=%s" f
  | Part p -> Fmt.pf ppf "part=%s" p
  | Env e -> Fmt.pf ppf "env=%s" e
  | Skip -> Fmt.string ppf "skip"
  | Non_det `Output -> Fmt.string ppf "non-deterministic=output"
  | Non_det `Command -> Fmt.string ppf "non-deterministic=command"
  | Version (op, v) ->
    Fmt.pf ppf "version%a%a" Relation.pp op Ocaml_version.pp v
  | Require_package p -> Fmt.pf ppf "require-package=%s" p
  | Set (v, x) -> Fmt.pf ppf "set-%s=%s" v x
  | Unset x -> Fmt.pf ppf "unset-%s" x

let is_prefix ~prefix s =
  let len_prefix = String.length prefix in
  if String.length s > len_prefix then
    String.equal (String.sub s 0 len_prefix) prefix
  else false

(* [is_prefix ~prefix s] is always checked before. *)
let split_prefix ~prefix s =
  let len_prefix = String.length prefix in
  String.sub s len_prefix (String.length s - len_prefix)

let doesnt_accept_value ~label ~value res =
  match value with
  | Some _ ->
    Error (`Msg (Format.sprintf "Label `%s` does not allow a value." label))
  | None -> Ok res

let requires_value ~label ~value f =
  match value with
  | Some (op, v) -> f op v
  | None -> Error (`Msg (Format.sprintf "Label `%s` requires a value." label))

let requires_eq ~label ~op ~value f =
  match op with
  | Relation.Eq -> Ok (f value)
  | _ ->
    let msg =
      Format.sprintf
        "Label `%s` requires assignment using the `=` operator." label
    in
    Error (`Msg msg)

let requires_eq_value ~label ~value f =
  requires_value ~label ~value (fun op value ->
      requires_eq ~label ~op ~value f)

let of_string s =
  let label, value = Relation.raw_parse s in
  match label with
  (* flags: labels without value *)
  | "skip" ->
    doesnt_accept_value ~label ~value Skip
  | v when is_prefix ~prefix:"unset-" v ->
    doesnt_accept_value ~label ~value (Unset (split_prefix ~prefix:"unset-" v))
  (* labels requiring a value and any comparison operator *)
  | "version" -> (
      requires_value ~label ~value (fun op v ->
          match Ocaml_version.of_string v with
          | Ok v -> Ok (Version (op, v))
          | Error (`Msg e) ->
            let msg = Format.sprintf "Invalid `version` label value: %s." e in
            Error (`Msg msg) ) )
  (* non-deterministic accepts any value *)
  | "non-deterministic" -> (
      match value with
      | Some (Relation.Eq, "output") -> Ok (Non_det `Output)
      | Some (Relation.Eq, "command") -> Ok (Non_det `Command)
      | Some (Relation.Eq, v) ->
        let msg =
          Format.sprintf
            "%S is not a valid value for label `%s`. Valid values are <none>,\
            \ %S and %S."
            v label "command" "output"
        in
        Error (`Msg msg)
      | Some _ ->
        let msg =
          Format.sprintf
            "label `%s` requires assignment using the `=` operator" label
        in
        Error (`Msg msg)
      | None -> Ok (Non_det `Output) )
  (* labels requiring a value and '=' operator *)
  | "dir" -> requires_eq_value ~label ~value (fun x -> Dir x)
  | "source-tree" -> requires_eq_value ~label ~value (fun x -> Source_tree x)
  | "file" -> requires_eq_value ~label ~value (fun x -> File x)
  | "part" -> requires_eq_value ~label ~value (fun x -> Part x)
  | "env" -> requires_eq_value ~label ~value (fun x -> Env x)
  | "require-package" ->
    requires_eq_value ~label ~value (fun x -> Require_package x)
  | l when is_prefix ~prefix:"set-" l ->
    requires_eq_value ~label ~value
      (fun x -> Set (split_prefix ~prefix:"set-" l, x))
  | l -> Error (`Msg (Format.sprintf "`%s` is not a valid label" l))

let of_string = function
  | "" -> Ok []
  | s ->
    let single_of_string x =
      match of_string x with
      | Ok label -> Ok [label]
      | Error msg -> Error [msg]
    in
    match String.split_on_char ',' s with
    | [] -> Error [`Msg "split_on_char should not return an empty list"]
    | [h] -> single_of_string h
    | h :: t ->
      let f acc s =
        match acc, of_string s with
        | Ok labels, Ok label -> Ok (label :: labels)
        | Error msgs, Ok _ -> Error msgs
        | Ok _, Error msg -> Error [msg]
        | Error msgs, Error msg -> Error (msg :: msgs)
      in
      match List.fold_left f (single_of_string h) t with
      | Ok labels -> Ok (List.rev labels)
      | Error msgs -> Error (List.rev msgs)
