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

type section = int * string

type value =
  | Raw
  | OCaml
  | Cram of { pad: int; tests: Cram.t list }
  | Toplevel of { pad: int; tests: Toplevel.t list }

type t = {
  line    : int;
  file    : string;
  section : section option;
  labels  : string list;
  header  : string option;
  contents: string list;
  value   : value;
}

let dump_string ppf s = Fmt.pf ppf "%S" s
let dump_strings = Fmt.Dump.list dump_string
let dump_section = Fmt.(Dump.pair int string)

let dump_value ppf = function
  | Raw -> Fmt.string ppf "Raw"
  | OCaml -> Fmt.string ppf "OCaml"
  | Cram { pad; tests } ->
    Fmt.pf ppf "@[Cram@ {pad=%d;@ tests=%a}@]"
      pad Fmt.(Dump.list Cram.dump) tests
  | Toplevel { pad; tests } ->
    Fmt.pf ppf "@[Toplevel { pad=%d;@ tests=%a}@]"
      pad Fmt.(Dump.list Toplevel.dump) tests

let dump ppf { file; line; section; labels; header; contents; value } =
  Fmt.pf ppf
    "{@[file: %s;@ line: %d;@ section: %a;@ labels: %a;@ header: %a;@
        contents: %a;@ value: %a@]}"
    file line
    Fmt.(Dump.option dump_section) section
    dump_strings labels
    Fmt.(Dump.option string) header
    Fmt.(Dump.list dump_string) contents
    dump_value value

let pp_lines pp = Fmt.(list ~sep:(unit "\n") pp)
let pp_contents ppf t = Fmt.pf ppf "%a\n" (pp_lines Fmt.string) t.contents
let pp_footer ppf () = Fmt.string ppf "```\n"

let pp_header ppf t =
  let pp_labels ppf () = match t.labels with
    | [] -> ()
    | l  -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit ",") string) l
  in
  Fmt.pf ppf "```%a%a\n" Fmt.(option string) t.header pp_labels ()

let pp ppf b =
  pp_header ppf b;
  pp_contents ppf b;
  pp_footer ppf ()

let labels = ["dir"; "non-deterministic"]

let check_labels t =
  List.for_all (fun l ->
      let l = List.filter (fun affix ->
          String.is_prefix ~affix:(affix ^ "=") l
        ) labels in
      List.length l < 2
    ) t.labels
  |> function
  | true  -> ()
  | false ->
    Fmt.failwith "invalid labels: '%a'"
      Fmt.(list ~sep:(unit ", ") string) t.labels

let get_label t label =
  let label' = label ^ "=" in
  match List.filter (String.is_prefix ~affix:label') t.labels with
  | []       -> None
  | _::_::_  -> Fmt.failwith "Too many labels: %s" label
  | [x]      -> match String.cut ~sep:label' x with
    | None   -> assert false
    | Some x -> Some (snd x)

let directory t = get_label t "dir"

let mode t = match get_label t "non-deterministic" with
  | None           -> `Normal
  | Some "output"  -> `Non_det `Output
  | Some "command" -> `Non_det `Command
  | Some s         -> Fmt.failwith "invalid mode: %s" s

let value t = t.value
let section t = t.section
let header t = t.header

let cram lines =
  let pad, tests = Cram.of_lines lines in
  Cram { pad; tests }

let is_raw_ocaml b =
  match b.header, b.contents with
  | Some "ocaml", h::_ ->
    let h = String.trim h in
    String.length h > 1 && h.[0] <> '#'
  | _ -> false

let toplevel ~file ~line lines =
  let pad, tests = Toplevel.of_lines ~line ~file lines in
  Toplevel { pad; tests }

let eval t =
  check_labels t;
  match t.header with
  | Some ("sh" | "bash") ->
    let value = cram t.contents in
    { t with value }
  | Some "ocaml" ->
    if is_raw_ocaml t then { t with value = OCaml }
    else
      let value = toplevel ~file:t.file ~line:t.line t.contents in
      { t with value }
  | _ -> t


let ends_by_semi_semi c = match List.rev c with
  | h::_ ->
    let len = String.length h in
    len > 2 && h.[len-1] = ';' && h.[len-2] = ';'
  | _ -> false

let pp_line_directive ppf (file, line) = Fmt.pf ppf "#%d %S" line file
let line_directive = Fmt.to_to_string pp_line_directive

let executable_contents b =
  let contents =
    if is_raw_ocaml b then b.contents
    else match b.value with
      | Raw | Cram _ -> []
      | OCaml -> line_directive (b.file, b.line) :: b.contents
      | Toplevel { tests; pad } ->
        List.flatten (
          List.map (fun t ->
              match Toplevel.command t with
              | [] -> []
              | cs ->
                let mk s = String.v ~len:(pad+2) (fun _ -> ' ') ^ s in
                line_directive (b.file, t.line) :: List.map mk cs
            ) tests)
  in
  if contents = [] || ends_by_semi_semi contents then contents
  else contents @ [";;"]
