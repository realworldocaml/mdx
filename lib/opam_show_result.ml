open Astring
module StrMap = Map.Make (String)

let empty = StrMap.empty

type t = string StrMap.t StrMap.t

(* Separate an opam show output line into two trimmed strings. *)
let parse_opam_show_line line =
  match String.cut ~sep:" " line with
  | None -> ("", "")
  | Some (field, value) -> (String.trim field, String.trim value)

(* Add a field -> value mapping to a package. *)
let add ~package ~field ~value t =
  StrMap.update package
    (function
      | None -> Some (StrMap.singleton field value)
      | Some fields -> Some (StrMap.add field value fields))
    t

(** [make lines] builds an Opam_show_result.t from an opam show output.
    The first field for each package must be 'name'*)
let make lines =
  let rec loop cur_name t = function
    | [] -> Ok t
    | line :: next -> (
        match (cur_name, parse_opam_show_line line) with
        | _, ("name", pkg_name) -> loop (Some pkg_name) t next
        | None, _ -> Error (`Msg "")
        | Some package, (field, value) ->
            let t = add ~package ~field ~value t in
            loop cur_name t next )
  in
  loop None empty lines

(** [get ~package ~field t] retrieves a field from a package. Returns
    None if the package or the field doesn't exist. *)
let get ~package ~field t =
  match StrMap.find_opt package t with None -> None | Some fields -> StrMap.find_opt field fields

(** [from_list bindings] builds an Opam_show_result.t from a list of
    bindings (package, field, value). *)
let from_list lst =
  List.fold_left
    (fun map (package, field, value) -> add ~package ~field ~value map)
    StrMap.empty lst

let bindings t =
  let bindings_map = StrMap.map StrMap.bindings t in
  StrMap.bindings bindings_map

(** Equality test on Opam_show_result.t *)
let equal = StrMap.equal (StrMap.equal String.equal)

(** Pretty-printer *)
let pp fmt t =
  let open Sexplib.Std in
  let sxp = [%sexp_of: (string * (string * string) list) list] (bindings t) in
  Sexplib.Sexp.pp_hum fmt sxp
