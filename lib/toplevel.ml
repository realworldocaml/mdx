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

let src = Logs.Src.create "ocaml-mdx"

module Log = (val Logs.src_log src : Logs.LOG)
open Astring

type t = {
  (* TODO: move vpad and hpad to `toplevel_tests` *)
  vpad : int;
  hpad : int;
  pos : Lexing.position;
  command : string list;
  output : Output.t list;
}

type toplevel_tests = { tests : t list; end_pad : string option }

let dump_line ppf = function
  | #Output.t as o -> Output.dump ppf o
  | `Command (c, _) -> Fmt.pf ppf "`Command %a" Fmt.Dump.(list string) c

let dump_lines = Fmt.Dump.list dump_line

let dump ppf { vpad; hpad; command; output; _ } =
  Fmt.pf ppf "@[{vpad=%d;@ hpad=%d;@ command=%a;@ output=%a}@]" vpad hpad
    Fmt.Dump.(list string)
    command
    (Fmt.Dump.list Output.dump)
    output

let dump_toplevel_tests ppf { tests; end_pad } =
  Fmt.pf ppf "@[{tests=%a;@ end_pad=%a}@]"
    Fmt.Dump.(list dump)
    tests
    Fmt.Dump.(option string)
    end_pad

let pp_vpad ppf t =
  let rec aux = function
    | 0 -> ()
    | i ->
        Fmt.pf ppf "\n";
        aux (i - 1)
  in
  aux t.vpad

(* somewhat idiosyncratic version of Fmt.list *)
let rec pp_list_string_nonblank ~sep ~blank ppf = function
  | [] -> ()
  | [ s ] -> Fmt.string ppf s
  | x :: y :: xs ->
      Fmt.string ppf x;
      let current_sep =
        match Util.String.all_blank y with true -> blank | false -> sep
      in
      current_sep ppf ();
      pp_list_string_nonblank ~sep ~blank ppf (y :: xs)

let pp_command ppf (t : t) =
  match t.command with
  | [] -> ()
  | l ->
      pp_vpad ppf t;
      let sep ppf () = Fmt.pf ppf "\n%a  " Pp.pp_pad t.hpad in
      let blank = Fmt.any "\n" in
      Fmt.pf ppf "%a# %a" Pp.pp_pad t.hpad
        (pp_list_string_nonblank ~sep ~blank)
        l

let pp ppf (t : t) =
  pp_command ppf t;
  Fmt.string ppf "\n";
  Pp.pp_lines (Output.pp ~pad:t.vpad) ppf t.output

let lexbuf ~(pos : Lexing.position) s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_start_p <- pos;
  lexbuf.lex_curr_p <- pos;
  lexbuf

let vpad_of_lines t =
  let rec aux i = function
    | `Output h :: t when Util.String.all_blank h -> aux (i + 1) t
    | t -> (i, t)
  in
  aux 0 t

let rec end_pad_of_lines = function
  | [] -> ([], None)
  | [ x; end_pad ] when Util.String.all_blank end_pad -> ([ x ], Some end_pad)
  | x :: xs ->
      let xs, end_pad = end_pad_of_lines xs in
      (x :: xs, end_pad)

let unpad hpad line =
  match Util.String.all_blank line with
  | true -> line
  | false ->
      if String.length line < hpad then Fmt.failwith "invalid padding: %S" line
      else String.with_index_range line ~first:hpad

let rec hpad_of_lines = function
  | [] -> 0
  | h :: hs -> (
      match Util.String.all_blank h with
      | true -> hpad_of_lines hs
      | false ->
          let i = ref 0 in
          while !i < String.length h && h.[!i] = ' ' do
            incr i
          done;
          !i)

let of_lines ~(loc : Location.t) t =
  let pos = loc.loc_start in
  (* Location.t considers the first line to be 1, whereas the tokenizer
     assumes lines start with 0. *)
  let pos = { pos with pos_lnum = pos.pos_lnum - 1 } in
  let hpad = hpad_of_lines t in
  let t, end_pad = end_pad_of_lines t in
  let lines = List.map (unpad hpad) t in
  let lines = String.concat ~sep:"\n" lines in
  let lxbuf = lexbuf ~pos lines in
  let lines = Lexer_top.token lxbuf in
  let vpad, lines = vpad_of_lines lines in
  Log.debug (fun l ->
      l "Toplevel.of_lines (vpad=%d, hpad=%d) %a" vpad hpad dump_lines lines);
  let mk vpad (command, (loc : Location.t)) output =
    { vpad; hpad; pos = loc.loc_start; command; output = List.rev output }
  in
  let rec aux vpad command_loc output acc = function
    | [] -> List.rev (mk vpad command_loc output :: acc)
    | (`Ellipsis as o) :: t -> aux vpad command_loc (o :: output) acc t
    | (`Output _ as o) :: t -> aux vpad command_loc (o :: output) acc t
    | `Command cmd :: t ->
        let vpad', output = vpad_of_lines output in
        aux vpad' cmd [] (mk vpad command_loc output :: acc) t
  in
  let tests =
    match lines with
    | `Command cmd :: t -> aux vpad cmd [] [] t
    | _ -> Fmt.failwith "invalid toplevel block: %a" Fmt.(Dump.list string) t
  in
  { tests; end_pad }
