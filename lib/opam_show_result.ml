open Astring

module StrMap = Map.Make(String)

let empty = StrMap.empty

type t = string StrMap.t StrMap.t

let parse_opam_show_line line =
  let drop = function
  | ':' -> true
  | x when Char.Ascii.is_white x -> true
  | _ -> false
  in
  match String.cut ~sep:" " line with
  | None -> "",""
  | Some (field, value) -> (String.trim ~drop field, String.trim value)

let add ~package ~field ~value t =
StrMap.update
    package
    (function
    | None -> Some (StrMap.singleton field value)
    | Some fields -> Some (StrMap.add field value fields))
    t

let make lines =
    let rec loop cur_name t = function
    | [] -> Ok t
    | line::next -> (
        match (cur_name, parse_opam_show_line line) with
        | (_, ("name", pkg_name)) -> loop (Some pkg_name) t next
        | (None, _) -> Error (`Msg "")
        | (Some package, (field, value)) ->
            let t = add ~package ~field ~value t in
            loop cur_name t next
    )
    in
    loop None empty lines


let get ~package ~field t =
    match StrMap.find_opt package t with
    | None -> None
    | Some fields -> StrMap.find_opt field fields

let from_list lst =
    List.fold_left
        (fun map (package,field,value)-> add ~package ~field ~value map)
        StrMap.empty
        lst

let bindings t =
    let bindings_map = StrMap.map StrMap.bindings t in
    StrMap.bindings bindings_map

let equal = StrMap.equal (StrMap.equal String.equal)



let pp fmt t =
    let open Sexplib.Std in
    let sxp = [%sexp_of: (string * (string * string) list) list] (bindings t) in
    Sexplib.Sexp.pp_hum fmt sxp
