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
  | Error of string list
  | Cram of { pad: int; tests: Cram.t list }
  | Toplevel of Toplevel.t list

type t = {
  line    : int;
  file    : string;
  section : section option;
  labels  : (string * string option) list;
  header  : string option;
  contents: string list;
  value   : value;
}

let dump_string ppf s = Fmt.pf ppf "%S" s
let dump_section = Fmt.(Dump.pair int string)

let dump_value ppf = function
  | Raw -> Fmt.string ppf "Raw"
  | OCaml -> Fmt.string ppf "OCaml"
  | Error e -> Fmt.pf ppf "Error %a" Fmt.(Dump.list dump_string) e
  | Cram { pad; tests } ->
    Fmt.pf ppf "@[Cram@ {pad=%d;@ tests=%a}@]"
      pad Fmt.(Dump.list Cram.dump) tests
  | Toplevel tests ->
    Fmt.pf ppf "@[Toplevel %a@]" Fmt.(Dump.list Toplevel.dump) tests

let dump_labels = Fmt.(Dump.list (pair dump_string Dump.(option dump_string)))

let dump ppf { file; line; section; labels; header; contents; value } =
  Fmt.pf ppf
    "{@[file: %s;@ line: %d;@ section: %a;@ labels: %a;@ header: %a;@
        contents: %a;@ value: %a@]}"
    file line
    Fmt.(Dump.option dump_section) section
    dump_labels labels
    Fmt.(Dump.option string) header
    Fmt.(Dump.list dump_string) contents
    dump_value value

let pp_lines pp = Fmt.(list ~sep:(unit "\n") pp)
let pp_contents ppf t = Fmt.pf ppf "%a\n" (pp_lines Fmt.string) t.contents
let pp_footer ppf () = Fmt.string ppf "```\n"

let pp_label ppf (k, v) = match v with
  | None   -> Fmt.string ppf k
  | Some v -> Fmt.pf ppf "%s=%s" k v

let pp_labels ppf = function
  | [] -> ()
  | l  -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit ",") pp_label) l

let pp_header ppf t =
  Fmt.pf ppf "```%a%a\n" Fmt.(option string) t.header pp_labels t.labels

let pp_error ppf b =
  match b.value with
  | Error e -> List.iter (fun e -> Fmt.pf ppf ">> @[<h>%a@]@." Fmt.words e) e
  | _ -> ()

let pp ppf b =
  pp_header ppf b;
  pp_error ppf b;
  pp_contents ppf b;
  pp_footer ppf ()

let labels = [
  "dir"              , [`Any];
  "file"             , [`Any];
  "part"             , [`Any];
  "env"              , [`Any];
  "non-deterministic", [`None; `Some "command"; `Some "output"]
]

let pp_value ppf = function
  | `Any   -> Fmt.string ppf "*"
  | `None   -> Fmt.string ppf "<none>"
  | `Some v -> dump_string ppf v

let match_label l p = match p, l with
  | `Any   , Some _ -> true
  | `None  , None   -> true
  | `Some p, Some l -> p=l
  | _ -> false

let pp_v ppf = function
  | None   -> Fmt.string ppf "<none>"
  | Some v -> Fmt.string ppf v

let rec pp_list pp ppf = function
  | []    -> ()
  | [x]   -> pp ppf x
  | [x;y] -> Fmt.pf ppf "%a and %a" pp x pp y
  | h::t  -> Fmt.pf ppf "%a, %a" pp h (pp_list pp) t

let check_labels t =
  List.fold_left (fun acc (k, v) ->
      try
        let vs = List.assoc k labels in
        if List.exists (match_label v) vs then acc
        else
          Fmt.strf "%a is not a valid value for label %S. \
                    Valid values are %a."
            pp_v v k (pp_list pp_value) vs
          :: acc
      with Not_found ->
        Fmt.strf "%S is not a valid label. \
                  Valid labels are are %a."
          k (pp_list dump_string) (List.map fst labels)
        :: acc
    ) [] t.labels
  |> function
  | [] -> Ok ()
  | es -> Result.Error es

let get_label t label =
  try Some (List.assoc label t.labels)
  with Not_found -> None

let directory t = match get_label t "dir" with
  | None   -> None
  | Some d -> d

let file t = match get_label t "file" with
  | None   -> None
  | Some f -> f

let part t = match get_label t "part" with
  | None   -> None
  | Some l -> l

let mode t =
  match get_label t "non-deterministic" with
  | None                  -> `Normal
  | Some None
  | Some (Some "output")  -> `Non_det `Output
  | Some (Some "command") -> `Non_det `Command
  | Some (Some _)         -> `Normal

let environment t = match get_label t "env" with
  | None
  | Some None
  | Some (Some "default") -> "default"
  | Some (Some s) -> s

let value t = t.value
let section t = t.section
let header t = t.header

let cram lines =
  let pad, tests = Cram.of_lines lines in
  Cram { pad; tests }

let is_raw_ocaml b =
  let rec aux = function
    | []     -> true
    | h :: t ->
      let h = String.trim h in
      if h = "" then aux t
      else String.length h > 1 && h.[0] <> '#'
  in
  match b.header, b.contents with
  | Some "ocaml", t -> aux t
  | _ -> false

let toplevel ~file ~line lines = Toplevel (Toplevel.of_lines ~line ~file lines)

let eval t =
  match check_labels t with
  | Error e -> { t with value = Error e }
  | Ok ()   ->
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
      | Error _ | Raw | Cram _ -> []
      | OCaml -> line_directive (b.file, b.line) :: b.contents
      | Toplevel tests ->
        List.flatten (
          List.map (fun t ->
              match Toplevel.command t with
              | [] -> []
              | cs ->
                let mk s = String.v ~len:(t.hpad+2) (fun _ -> ' ') ^ s in
                line_directive (b.file, t.line) :: List.map mk cs
            ) tests)
  in
  if contents = [] || ends_by_semi_semi contents then contents
  else contents @ [";;"]

let labels_of_string s =
  let labels = String.cuts ~empty:false ~sep:"," s in
  List.map (fun s ->
      match String.cut ~sep:"=" s with
      | None        -> s, None
      | Some (k, v) -> k, Some v
    ) labels
