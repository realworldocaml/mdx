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

open Result
open Compat

module Header = struct
  type t = Shell | OCaml | Other of string

  let pp ppf = function
    | Shell -> Fmt.string ppf "sh"
    | OCaml -> Fmt.string ppf "ocaml"
    | Other s -> Fmt.string ppf s

  let of_string = function
    | "" -> None
    | "sh" | "bash" -> Some Shell
    | "ocaml" -> Some OCaml
    | s -> Some (Other s)
end

type section = int * string

type cram_value = { pad : int; tests : Cram.t list }

type value =
  | Raw
  | OCaml
  | Error of string list
  | Cram of cram_value
  | Toplevel of Toplevel.t list

type raw = {
  line : int;
  file : string;
  section : section option;
  labels : Label.t list;
  header : Header.t option;
  contents : string list;
  value : value;
}

type t =
  | Toplevel of raw
  | Shell of raw
  | Include of raw
  | OCaml of raw

let apply_raw f = function
  | Toplevel r -> f r
  | Shell r -> f r
  | Include r -> f r
  | OCaml r -> f r

let line = apply_raw (fun t -> t.line)
let filename = apply_raw (fun t -> t.file)
let section = apply_raw (fun t -> t.section)
let labels = apply_raw (fun t -> t.labels)
let header = apply_raw (fun t -> t.header)
let contents = apply_raw (fun t -> t.contents)
let value = apply_raw (fun t -> t.value)

let dump_string ppf s = Fmt.pf ppf "%S" s

let dump_section = Fmt.(Dump.pair int string)

let dump_value ppf = function
  | Raw -> Fmt.string ppf "Raw"
  | OCaml -> Fmt.string ppf "OCaml"
  | Error e -> Fmt.pf ppf "Error %a" Fmt.(Dump.list dump_string) e
  | Cram { pad; tests } ->
      Fmt.pf ppf "@[Cram@ {pad=%d;@ tests=%a}@]" pad
        Fmt.(Dump.list Cram.dump)
        tests
  | Toplevel tests ->
      Fmt.pf ppf "@[Toplevel %a@]" Fmt.(Dump.list Toplevel.dump) tests

let dump ppf t =
  Fmt.pf ppf
    "{@[file: %s;@ line: %d;@ section: %a;@ labels: %a;@ header: %a;@\n\
    \        contents: %a;@ value: %a@]}" (filename t) (line t)
    Fmt.(Dump.option dump_section)
    (section t)
    Fmt.Dump.(list Label.pp)
    (labels t)
    Fmt.(Dump.option Header.pp)
    (header t)
    Fmt.(Dump.list dump_string)
    (contents t) dump_value (value t)

let pp_lines syntax =
  let pp =
    match syntax with Some Syntax.Cram -> Fmt.fmt "  %s" | _ -> Fmt.string
  in
  Fmt.(list ~sep:(unit "\n") pp)

let pp_contents ?syntax ppf t = Fmt.pf ppf "%a\n" (pp_lines syntax) (contents t)

let pp_footer ?syntax ppf () =
  match syntax with Some Syntax.Cram -> () | _ -> Fmt.string ppf "```\n"

let pp_labels ppf = function
  | [] -> ()
  | l -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit ",") Label.pp) l

let pp_header ?syntax ppf t =
  match syntax with
  | Some Syntax.Cram -> (
      match labels t with
      | [] -> ()
      | [Non_det None] -> Fmt.pf ppf "<-- non-deterministic\n"
      | [Non_det (Some Nd_output)] ->
        Fmt.pf ppf "<-- non-deterministic output\n"
      | [Non_det (Some Nd_command)] ->
        Fmt.pf ppf "<-- non-deterministic command\n"
      | _ -> failwith "cannot happen: checked during parsing" )
  | _ ->
    Fmt.pf ppf "```%a%a\n" Fmt.(option Header.pp) (header t) pp_labels
      (labels t)

let pp_error ppf b =
  match value b with
  | Error e -> List.iter (fun e -> Fmt.pf ppf ">> @[<h>%a@]@." Fmt.words e) e
  | _ -> ()

let pp ?syntax ppf b =
  pp_header ?syntax ppf b;
  pp_error ppf b;
  pp_contents ?syntax ppf b;
  pp_footer ?syntax ppf ()

let get_label f t = Util.List.find_map f (labels t)

let get_label_or f ~default t =
  Util.Option.value ~default (Util.List.find_map f (labels t))

let directory t = get_label (function Dir x -> Some x | _ -> None) t

let file t = get_label (function File x -> Some x | _ -> None) t

let part t = get_label (function Part x -> Some x | _ -> None) t

let version t =
  get_label (function Version (x, y) -> Some (x, y) | _ -> None) t

let source_trees t =
  List.filter_map
    (function Label.Source_tree x -> Some x | _ -> None)
    (labels t)

let mode t =
  get_label_or
    (function
      | Non_det (Some mode) -> Some (`Non_det mode)
      | Non_det None -> Some (`Non_det Label.default_non_det)
      | _ -> None)
    ~default:`Normal t

let skip t = List.exists (function Label.Skip -> true | _ -> false) (labels t)

let environment t =
  get_label_or
    (function Label.Env e -> Some e | _ -> None)
    ~default:"default" t

let set_variables t =
  List.filter_map
    (function Label.Set (v, x) -> Some (v, x) | _ -> None)
    (labels t)

let unset_variables t =
  List.filter_map (function Label.Unset x -> Some x | _ -> None) (labels t)

let explicit_required_packages t =
  List.filter_map
    (function Label.Require_package x -> Some x | _ -> None)
    (labels t)

let require_re =
  let open Re in
  seq [ str "#require \""; group (rep1 any); str "\"" ]

let require_from_line line =
  let open Util.Result.Infix in
  let re = Re.compile require_re in
  match Re.exec_opt re line with
  | None -> Ok Library.Set.empty
  | Some group ->
      let matched = Re.Group.get group 1 in
      let libs_str = String.split_on_char ',' matched in
      Util.Result.List.map ~f:Library.from_string libs_str >>| fun libs ->
      Library.Set.of_list libs

let require_from_lines lines =
  let open Util.Result.Infix in
  Util.Result.List.map ~f:require_from_line lines >>| fun libs ->
  List.fold_left Library.Set.union Library.Set.empty libs

let required_libraries = function
  | { value = Toplevel _; contents; _ } -> require_from_lines contents
  | { value = Raw | OCaml | Error _ | Cram _; _ } -> Ok Library.Set.empty

let required_libraries = apply_raw required_libraries

let cram lines =
  let pad, tests = Cram.of_lines lines in
  Cram { pad; tests }

let guess_ocaml_kind b =
  let rec aux = function
    | [] -> `Code
    | h :: t ->
        let h = String.trim h in
        if h = "" then aux t
        else if String.length h > 1 && h.[0] = '#' then `Toplevel
        else `Code
  in
  aux b.contents

let eval t =
  match t.header with
  | Some Shell -> { t with value= cram t.contents }
  | Some OCaml -> (
      match guess_ocaml_kind t with
      | `Code -> { t with value = OCaml }
      | `Toplevel ->
          let file = t.file and line = t.line in
          { t with value= Toplevel (Toplevel.of_lines ~file ~line t.contents) } )
  | _ -> t

let eval = function
  | Toplevel r -> Toplevel (eval r)
  | Shell r -> Shell (eval r)
  | Include r -> Include (eval r)
  | OCaml r -> OCaml (eval r)

let ends_by_semi_semi c =
   match List.rev c with
  | h :: _ ->
      let len = String.length h in
      len > 2 && h.[len - 1] = ';' && h.[len - 2] = ';'
  | _ -> false

let pp_line_directive ppf (file, line) = Fmt.pf ppf "#%d %S" line file

let line_directive = Fmt.to_to_string pp_line_directive

let executable_contents b =
  let contents =
    match b with
    | OCaml _ -> contents b
    | _ ->
      match value b with
      | Error _ | Raw | Cram _ -> []
      | OCaml -> line_directive (filename b, line b) :: (contents b)
      | Toplevel tests ->
          List.flatten (
            List.map (fun t ->
              match Toplevel.command t with
              | [] -> []
              | cs ->
                let mk s = String.make (t.hpad+2) ' ' ^ s in
                line_directive (filename b, t.line) :: List.map mk cs
            ) tests )
  in
  if contents = [] || ends_by_semi_semi contents then contents
  else contents @ [ ";;" ]

let version_enabled t =
  match Ocaml_version.of_string Sys.ocaml_version with
  | Ok curr_version -> (
      match version t with
      | Some (op, v) ->
          Label.Relation.compare op (Ocaml_version.compare curr_version v) 0
      | None -> true )
  | Error (`Msg e) -> Fmt.failwith "invalid OCaml version: %s" e

let mk ~line ~file ~section ~labels ~header ~contents ~value =
  let raw = { line; file; section; labels; header; contents; value } in
  if List.exists (function Label.File _ -> true | _ -> false) labels then
    Include raw
  else
    match header with
    | Some Shell -> Shell raw
    | _ ->
      match guess_ocaml_kind raw with
      | `Toplevel -> Toplevel raw
      | `Code -> OCaml raw

let is_active ?section:s t =
  let active =
    match s with
    | Some p -> (
        match section t with
        | Some s -> Re.execp (Re.Perl.compile_pat p) (snd s)
        | None -> Re.execp (Re.Perl.compile_pat p) "" )
    | None -> true
  in
  active && version_enabled t && not (skip t)
