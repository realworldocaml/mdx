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

open Astring

type t =
  | Dir of string
  | Source_tree of string
  | File of string
  | Part of string
  | Env of string
  | Skip
  | Non_det of [`Output | `Command]
  | Version of [`Eq | `Neq | `Le | `Lt | `Ge | `Gt] * Ocaml_version.t
  | Require_package of string
  | Set of string * string
  | Unset of string

let pp_relation ppf = function
  | `Eq -> Fmt.string ppf "="
  | `Neq -> Fmt.string ppf "<>"
  | `Gt -> Fmt.string ppf ">"
  | `Ge -> Fmt.string ppf ">="
  | `Lt -> Fmt.string ppf "<"
  | `Le -> Fmt.string ppf "<="

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
    Fmt.pf ppf "version%a%a" pp_relation op Ocaml_version.pp v
  | Require_package p -> Fmt.pf ppf "require-package=%s" p
  | Set (v, x) -> Fmt.pf ppf "set-%s=%s" v x
  | Unset x -> Fmt.pf ppf "unset-%s" x

let split sep s op =
  match String.cut ~sep s with
  | None -> s, None (* operator does not matter here *)
  | Some (k, v) -> k, Some (op, v)

let ( ||| ) x y =
  match x with
  | _, None -> y
  | x -> x

let split_prefix ~prefix s =
  match String.cut ~sep:prefix s with
  | Some ("", v) when not (String.is_empty v) -> v
  | _ -> Fmt.failwith "invalid label `%s`" s

let doesnt_accept_value ~label ~value res =
  match value with
  | Some _ -> Fmt.failwith "label `%s` does not allow a value" label
  | None -> res

let requires_value ~label ~value f =
  match value with
  | Some (op, v) -> f op v
  | None -> Fmt.failwith "label `%s` requires a value" label

let requires_eq_value ~label ~value f =
  match value with
  | Some (`Eq, v) -> f v
  | Some (_, _) ->
    Fmt.failwith "label `%s` requires assignment using the `=` operator" label
  | None -> Fmt.failwith "label `%s` requires a value" label

let of_string s =
  let label, value =
    split "<>" s `Neq |||
    split ">=" s `Ge |||
    split ">"  s `Gt |||
    split "<=" s `Le |||
    split "<"  s `Lt |||
    split "="  s `Eq
  in
  match label with
  (* flags: labels without value *)
  | "skip" ->
    doesnt_accept_value ~label ~value Skip
  | v when String.is_prefix ~affix:"unset-" v ->
    doesnt_accept_value ~label ~value (Unset (split_prefix ~prefix:"unset-" v))
  (* labels requiring a value and any comparison operator *)
  | "version" -> (
      requires_value ~label ~value (fun op v ->
          match Ocaml_version.of_string v with
          | Ok v -> Version (op, v)
          | Error (`Msg e) ->
            Fmt.failwith "invalid `version` label value: %s" e ) )
  (* non-deterministic accepts any value *)
  | "non-deterministic" -> (
      match value with
      | Some (`Eq, "output") -> Non_det `Output
      | Some (`Eq, "command") -> Non_det `Command
      | Some (`Eq, v) ->
        Fmt.failwith
          "%S is not a valid value for label `%s`. Valid values are <none>,\
          \ %S and %S."
          v label "command" "output"
      | Some _ ->
        Fmt.failwith
          "label `%s` requires assignment using the `=` operator" label
      | None -> Non_det `Output )
  (* labels requiring a value and '=' operator *)
  | "dir" -> requires_eq_value ~label ~value (fun x -> Dir x)
  | "source-tree" -> requires_eq_value ~label ~value (fun x -> Source_tree x)
  | "file" -> requires_eq_value ~label ~value (fun x -> File x)
  | "part" -> requires_eq_value ~label ~value (fun x -> Part x)
  | "env" -> requires_eq_value ~label ~value (fun x -> Env x)
  | "require-package" ->
    requires_eq_value ~label ~value (fun x -> Require_package x)
  | l when String.is_prefix ~affix:"set-" l ->
    requires_eq_value ~label ~value
      (fun x -> Set (split_prefix ~prefix:"set-" l, x))
  | l -> Fmt.failwith "`%s` is not a valid label" l

let of_string s =
  if String.is_empty s then []
  else
    let labels = String.cuts ~empty:false ~sep:"," s in
    List.map of_string labels
