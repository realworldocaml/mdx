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

  let of_string = function
    | "<>" -> Neq
    | ">=" -> Ge
    | ">" -> Gt
    | "<=" -> Le
    | "<" -> Lt
    | "=" -> Eq
    | _ -> (* can not happen, filtered by the regexp *) assert false

  let re =
    let open Re in
    compile
    @@ seq
         [
           bos;
           group (rep (alt [ alnum; char '-'; char '_' ]));
           group
             (alt [ str "<="; str ">="; str "<>"; str "<"; str ">"; str "=" ]);
           group (rep any);
           eos;
         ]

  let raw_parse s =
    match Re.exec_opt re s with
    | None -> (s, None)
    | Some g -> (
        try
          let label = Re.Group.get g 1 in
          let op = of_string (Re.Group.get g 2) in
          let value = Re.Group.get g 3 in
          (label, Some (op, value))
        with Not_found -> (s, None))
end

type non_det = Nd_output | Nd_command

let default_non_det = Nd_output

type block_type = OCaml | Cram | Toplevel | Include

let pp_block_type ppf = function
  | OCaml -> Fmt.string ppf "ocaml"
  | Cram -> Fmt.string ppf "cram"
  | Toplevel -> Fmt.string ppf "toplevel"
  | Include -> Fmt.string ppf "include"

type t =
  | Dir of string
  | Source_tree of string
  | File of string
  | Part of string
  | Env of string
  | Skip
  | Non_det of non_det option
  | Version of Relation.t * Ocaml_version.t
  | Set of string * string
  | Unset of string
  | Block_type of block_type

let pp ppf = function
  | Dir d -> Fmt.pf ppf "dir=%s" d
  | Source_tree s -> Fmt.pf ppf "source-tree=%s" s
  | File f -> Fmt.pf ppf "file=%s" f
  | Part p -> Fmt.pf ppf "part=%s" p
  | Env e -> Fmt.pf ppf "env=%s" e
  | Skip -> Fmt.string ppf "skip"
  | Non_det None -> Fmt.string ppf "non-deterministic"
  | Non_det (Some Nd_output) -> Fmt.string ppf "non-deterministic=output"
  | Non_det (Some Nd_command) -> Fmt.string ppf "non-deterministic=command"
  | Version (op, v) ->
      Fmt.pf ppf "version%a%a" Relation.pp op Ocaml_version.pp v
  | Set (v, x) -> Fmt.pf ppf "set-%s=%s" v x
  | Unset x -> Fmt.pf ppf "unset-%s" x
  | Block_type bt -> Fmt.pf ppf "block-type=%a" pp_block_type bt

let is_prefix ~prefix s =
  let len_prefix = String.length prefix in
  if String.length s > len_prefix then
    String.equal (String.sub s 0 len_prefix) prefix
  else false

(* [is_prefix ~prefix s] is always checked before. *)
let split_prefix ~prefix s =
  let len_prefix = String.length prefix in
  String.sub s len_prefix (String.length s - len_prefix)

let non_eq_op ~label =
  Util.Result.errorf "Label `%s` requires assignment using the `=` operator."
    label

let invalid_value ~label ~allowed_values value =
  Util.Result.errorf
    "%S is not a valid value for label `%s`. Valid values are %s." value label
    (Util.String.english_conjonction allowed_values)

let doesnt_accept_value ~label ~value res =
  match value with
  | Some _ -> Util.Result.errorf "Label `%s` does not allow a value." label
  | None -> Ok res

let requires_value ~label ~value f =
  match value with
  | Some (op, v) -> f op v
  | None -> Util.Result.errorf "Label `%s` requires a value." label

let requires_eq_value ~label ~value f =
  requires_value ~label ~value (fun op value ->
      match op with Relation.Eq -> f value | _ -> non_eq_op ~label)

let version_of_string s =
  match Ocaml_version.of_string s with
  | Ok v -> Ok v
  | Error (`Msg e) -> Util.Result.errorf "Invalid version: %s." e

let parse_non_det_value ~label s =
  match s with
  | "output" -> Ok Nd_output
  | "command" -> Ok Nd_command
  | s ->
      let allowed_values = [ "<none>"; {|"command"|}; {|"output"|} ] in
      invalid_value ~label ~allowed_values s

let parse_block_type_value ~label s =
  match s with
  | "ocaml" -> Ok OCaml
  | "cram" -> Ok Cram
  | "toplevel" -> Ok Toplevel
  | "include" -> Ok Include
  | s ->
      let allowed_values =
        [ {|"ocaml"|}; {|"cram"|}; {|"toplevel"|}; {|"include"|} ]
      in
      invalid_value ~label ~allowed_values s

let interpret label value =
  let open Util.Result.Infix in
  match label with
  | "skip" -> doesnt_accept_value ~label ~value Skip
  | "ocaml" -> doesnt_accept_value ~label ~value (Block_type OCaml)
  | "cram" -> doesnt_accept_value ~label ~value (Block_type Cram)
  | "toplevel" -> doesnt_accept_value ~label ~value (Block_type Toplevel)
  | "include" -> doesnt_accept_value ~label ~value (Block_type Include)
  | v when is_prefix ~prefix:"unset-" v ->
      doesnt_accept_value ~label ~value
        (Unset (split_prefix ~prefix:"unset-" v))
  | "version" ->
      requires_value ~label ~value (fun op v ->
          version_of_string v >>= fun v -> Ok (Version (op, v)))
  | "non-deterministic" -> (
      match value with
      | None -> Ok (Non_det None)
      | Some (Relation.Eq, s) ->
          parse_non_det_value ~label s >>= fun nd -> Ok (Non_det (Some nd))
      | Some _ -> non_eq_op ~label)
  | "dir" -> requires_eq_value ~label ~value (fun x -> Ok (Dir x))
  | "source-tree" ->
      requires_eq_value ~label ~value (fun x -> Ok (Source_tree x))
  | "file" -> requires_eq_value ~label ~value (fun x -> Ok (File x))
  | "part" -> requires_eq_value ~label ~value (fun x -> Ok (Part x))
  | "env" -> requires_eq_value ~label ~value (fun x -> Ok (Env x))
  | "block-type" ->
      requires_eq_value ~label ~value (fun x ->
          parse_block_type_value ~label x >>= fun bt -> Ok (Block_type bt))
  | l when is_prefix ~prefix:"set-" l ->
      requires_eq_value ~label ~value (fun x ->
          Ok (Set (split_prefix ~prefix:"set-" l, x)))
  | l -> Error (`Msg (Format.sprintf "`%s` is not a valid label." l))

let of_string s =
  let f acc s =
    let label, value = Relation.raw_parse s in
    match (acc, interpret label value) with
    | Ok labels, Ok label -> Ok (label :: labels)
    | Error msgs, Ok _ -> Error msgs
    | Ok _, Error msg -> Error [ msg ]
    | Error msgs, Error msg -> Error (msg :: msgs)
  in
  match s with
  | "" -> Ok []
  | s -> (
      let split = String.split_on_char ',' s in
      match List.fold_left f (Ok []) split with
      | Ok labels -> Ok (List.rev labels)
      | Error msgs -> Error (List.rev msgs))
