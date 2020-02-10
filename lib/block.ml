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

type t = {
  line : int;
  file : string;
  section : section option;
  labels : Label.t list;
  header : Header.t option;
  contents : string list;
  value : value;
}

let line t = t.line
let filename t = t.file
let section t = t.section
let contents t = t.contents
let value t = t.value

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

let dump ppf { file; line; section; labels; header; contents; value } =
  Fmt.pf ppf
    "{@[file: %s;@ line: %d;@ section: %a;@ labels: %a;@ header: %a;@\n\
    \        contents: %a;@ value: %a@]}" file line
    Fmt.(Dump.option dump_section)
    section
    Fmt.Dump.(list Label.pp)
    labels
    Fmt.(Dump.option Header.pp)
    header
    Fmt.(Dump.list dump_string)
    contents dump_value value

let pp_lines syntax =
  let pp =
    match syntax with Some Syntax.Cram -> Fmt.fmt "  %s" | _ -> Fmt.string
  in
  Fmt.(list ~sep:(unit "\n") pp)

let pp_contents ?syntax ppf t = Fmt.pf ppf "%a\n" (pp_lines syntax) t.contents

let pp_footer ?syntax ppf () =
  match syntax with Some Syntax.Cram -> () | _ -> Fmt.string ppf "```\n"

let pp_labels ppf = function
  | [] -> ()
  | l -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit ",") Label.pp) l

let pp_header ?syntax ppf t =
  match syntax with
  | Some Syntax.Cram -> (
      match t.labels with
      | [] -> ()
      | [Non_det None] -> Fmt.pf ppf "<-- non-deterministic\n"
      | [Non_det (Some Nd_output)] ->
        Fmt.pf ppf "<-- non-deterministic output\n"
      | [Non_det (Some Nd_command)] ->
        Fmt.pf ppf "<-- non-deterministic command\n"
      | _ -> failwith "cannot happen: checked during parsing" )
  | _ ->
    Fmt.pf ppf "```%a%a\n" Fmt.(option Header.pp) t.header pp_labels t.labels

let pp_error ppf b =
  match b.value with
  | Error e -> List.iter (fun e -> Fmt.pf ppf ">> @[<h>%a@]@." Fmt.words e) e
  | _ -> ()

let pp ?syntax ppf b =
  pp_header ?syntax ppf b;
  pp_error ppf b;
  pp_contents ?syntax ppf b;
  pp_footer ?syntax ppf ()

let get_label f t = Util.List.find_map f t.labels

let get_label_or f ~default t =
  Util.Option.value ~default (Util.List.find_map f t.labels)

let directory t = get_label (function Dir x -> Some x | _ -> None) t

let file t = get_label (function File x -> Some x | _ -> None) t

let part t = get_label (function Part x -> Some x | _ -> None) t

let version t =
  get_label (function Version (x, y) -> Some (x, y) | _ -> None) t

let source_trees t =
  List.filter_map
    (function Label.Source_tree x -> Some x | _ -> None)
    t.labels

let mode t =
  get_label_or
    (function
      | Non_det (Some mode) -> Some (`Non_det mode)
      | Non_det None -> Some (`Non_det Label.default_non_det)
      | _ -> None)
    ~default:`Normal t

let skip t = List.exists (function Label.Skip -> true | _ -> false) t.labels

let environment t =
  get_label_or
    (function Label.Env e -> Some e | _ -> None)
    ~default:"default" t

let set_variables t =
  List.filter_map
    (function Label.Set (v, x) -> Some (v, x) | _ -> None)
    t.labels

let unset_variables t =
  List.filter_map (function Label.Unset x -> Some x | _ -> None) t.labels

let explicit_required_packages t =
  List.filter_map
    (function Label.Require_package x -> Some x | _ -> None)
    t.labels

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

let ends_by_semi_semi c =
  match List.rev c with
  | h :: _ ->
      let len = String.length h in
      len > 2 && h.[len - 1] = ';' && h.[len - 2] = ';'
  | _ -> false

let pp_line_directive ppf (file, line) = Fmt.pf ppf "#%d %S" line file

let line_directive = Fmt.to_to_string pp_line_directive

let executable_contents b =
  let whole_content =
    match b.header with
    | Some OCaml -> (
        match guess_ocaml_kind b with
        | `Code -> true
        | `Toplevel -> false )
    | _ -> false
  in
  let contents =
    if whole_content then b.contents
    else
      match b.value with
      | Error _ | Raw | Cram _ -> []
      | OCaml -> line_directive (b.file, b.line) :: b.contents
      | Toplevel tests ->
          List.flatten (
            List.map (fun t ->
              match Toplevel.command t with
              | [] -> []
              | cs ->
                let mk s = String.make (t.hpad+2) ' ' ^ s in
                line_directive (b.file, t.line) :: List.map mk cs
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
  { line; file; section; labels; header; contents; value }
