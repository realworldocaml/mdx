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
  labels  : Label.t list;
  header  : Header.t option;
  contents: string list;
  value   : value;
  non_det : Label.non_det option;
}

type include_block = {
  line : int;
  file : string;
  section : section option;
  labels : Label.t list;
  header : Header.t option;
  contents : string list;
  value : value;
  file_included : string;
  part_included : string option;
}

type t =
  | Toplevel of raw
  | Shell of raw
  | Include of include_block
  | OCaml of raw

let line = function
  | Toplevel t -> t.line
  | Shell t -> t.line
  | Include t -> t.line
  | OCaml t -> t.line

let filename = function
  | Toplevel t -> t.file
  | Shell t -> t.file
  | Include t -> t.file
  | OCaml t -> t.file

let section = function
  | Toplevel t -> t.section
  | Shell t -> t.section
  | Include t -> t.section
  | OCaml t -> t.section

let labels = function
  | Toplevel t -> t.labels
  | Shell t -> t.labels
  | Include t -> t.labels
  | OCaml t -> t.labels

let header = function
  | Toplevel t -> t.header
  | Shell t -> t.header
  | Include t -> t.header
  | OCaml t -> t.header

let contents = function
  | Toplevel t -> t.contents
  | Shell t -> t.contents
  | Include t -> t.contents
  | OCaml t -> t.contents

let value = function
  | Toplevel t -> t.value
  | Shell t -> t.value
  | Include t -> t.value
  | OCaml t -> t.value

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

let file = function Include t -> Some t.file_included | _ -> None

let version t = get_label (function Version (x, y) -> Some (x, y) | _ -> None) t

let source_trees t =
  List.filter_map
    (function Label.Source_tree x -> Some x | _ -> None)
    (labels t)

let mode t = get_label (function Label.Non_det mode -> mode | _ -> None) t

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

let required_libraries t =
  match value t with
  | Toplevel _ -> require_from_lines (contents t)
  | Raw | OCaml | Error _ | Cram _ -> Ok Library.Set.empty

let cram lines =
  let pad, tests = Cram.of_lines lines in
  Cram { pad; tests }

let guess_ocaml_kind contents =
  let rec aux = function
    | [] -> `Code
    | h :: t ->
        let h = String.trim h in
        if h = "" then aux t
        else if String.length h > 1 && h.[0] = '#' then `Toplevel
        else `Code
  in
  aux contents

let update_value t value =
  match t with
  | Toplevel b -> Toplevel { b with value }
  | Shell b -> Shell { b with value }
  | Include b -> Include { b with value }
  | OCaml b -> OCaml { b with value }

let eval t =
  let value =
    match header t with
    | Some Shell -> cram (contents t)
    | Some OCaml -> (
        match guess_ocaml_kind (contents t) with
        | `Code -> OCaml
        | `Toplevel ->
          let file = filename t and line = line t in
          Toplevel (Toplevel.of_lines ~file ~line (contents t)) )
    | _ -> value t
  in
  update_value t value

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

let get_label f (labels : Label.t list) =
  let rec aux = function
    | [] -> None
    | h :: t ->
      match f h with
      | Some x -> Some x
      | None -> aux t
  in
  aux labels

let check_not_set msg = function
  | Some _ -> Util.Result.errorf msg
  | None -> Ok ()

let mk ~line ~file ~section ~labels ~header ~contents ~value =
  let non_det = get_label (function Non_det x -> x | _ -> None) labels in
  let part = get_label (function Part x -> Some x | _ -> None) labels in
  let open Util.Result.Infix in
  match get_label (function File x -> Some x | _ -> None) labels with
  | Some file_included -> (
      check_not_set
        "`non-deterministic` label cannot be used with a `file` label." non_det
      >>= fun () ->
      match part with
      | Some part -> (
          match header with
          | Some Header.OCaml ->
            Ok
              (Include
                 { line; file; section; labels; header; contents; value;
                   file_included; part_included= Some part } )
          | _ ->
            Util.Result.errorf
              "`part` is not supported for non-OCaml code blocks." )
      | None ->
        Ok
          (Include
             { line; file; section; labels; header; contents; value;
               file_included; part_included= None } ) )
  | None ->
    check_not_set "`part` label requires a `file` label." part >>= fun () ->
    let raw =
      { line; file; section; labels; header; contents; value; non_det }
    in
    match header with
    | Some Shell -> Ok (Shell raw)
    | _ ->
      match guess_ocaml_kind contents with
      | `Toplevel -> Ok (Toplevel raw)
      | `Code -> Ok (OCaml raw)

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
