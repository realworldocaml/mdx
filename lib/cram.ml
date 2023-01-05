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

type t = { command : string list; output : Output.t list; exit_code : int }

type cram_tests = {
  start_pad : int;
  hpad : int;
  tests : t list;
  end_pad : string option;
}

let dump_line ppf = function
  | #Output.t as o -> Output.dump ppf o
  | `Exit i -> Fmt.pf ppf "`Exit %d" i
  | `Command c -> Fmt.pf ppf "`Command %S" c
  | `Command_first c -> Fmt.pf ppf "`Command_first %S" c
  | `Command_cont c -> Fmt.pf ppf "`Command_cont %S" c
  | `Command_last c -> Fmt.pf ppf "`Command_last %S" c

let dump ppf { command; output; exit_code } =
  Fmt.pf ppf "{@[command: %a;@ output: %a;@ exit_code: %d]}"
    Fmt.Dump.(list string)
    command
    (Fmt.Dump.list Output.dump)
    output exit_code

let rec pp_vertical_pad ppf = function
  | 0 -> ()
  | n ->
      Fmt.pf ppf "\n";
      pp_vertical_pad ppf (Int.pred n)

let pp_command ?(pad = 0) ppf (t : t) =
  match t.command with
  | [] -> ()
  | l ->
      let sep ppf () = Fmt.pf ppf "\\\n%a> " Pp.pp_pad pad in
      Fmt.pf ppf "%a$ %a" Pp.pp_pad pad Fmt.(list ~sep string) l

let pp_exit_code ?(pad = 0) ppf = function
  | 0 -> ()
  | n -> Fmt.pf ppf "\n%a[%d]" Pp.pp_pad pad n

let pp ?pad ppf (t : t) =
  pp_command ?pad ppf t;
  Fmt.string ppf "\n";
  Pp.pp_lines (Output.pp ?pad) ppf t.output;
  pp_exit_code ?pad ppf t.exit_code

let hpad_of_lines = function
  | [] -> 0
  | h :: _ ->
      let i = ref 0 in
      while !i < String.length h && h.[!i] = ' ' do
        incr i
      done;
      !i

let unpad_line ~hpad line =
  match Util.String.all_blank line with
  | true -> String.with_index_range line ~first:hpad
  | false -> (
      match String.length line < hpad with
      | true -> Fmt.failwith "invalid padding: %S" line
      | false -> String.with_index_range line ~first:hpad)

let unpad hpad = List.map (unpad_line ~hpad)

let dump_cram_tests ppf { start_pad; hpad; tests; end_pad } =
  Fmt.pf ppf "{@[start_pad: %d;@ hpad: %d;@ tests: %a;@ end_pad: %a]}" start_pad
    hpad
    Fmt.Dump.(list dump)
    tests
    Fmt.Dump.(option string)
    end_pad

(* determine the amount of empty lines before the first non-empty line *)
let start_pad lines =
  let pad_lines, code_lines =
    Util.List.partition_until (String.equal "") lines
  in
  (* make sure there *are* non-empty lines in the first place *)
  match List.length code_lines with
  | 0 -> (0, lines)
  | _ -> (List.length pad_lines, code_lines)

let rec end_pad = function
  | [] -> (None, [])
  | [ x; last ] when Util.String.all_blank last -> (Some last, [ x ])
  | x :: xs ->
      let pad, xs = end_pad xs in
      (pad, x :: xs)

type cram_input = {
  start_pad : int;
  tests : string list;
  end_pad : string option;
}

let determine_padding lines =
  match List.length lines with
  | 0 -> failwith "unable to determine padding, no lines in block"
  (* one line, it doesn't have any paddings *)
  | 1 -> { start_pad = 0; tests = lines; end_pad = None }
  | _ ->
      let start_pad, lines = start_pad lines in
      let end_pad, lines = end_pad lines in
      let lines =
        match List.for_all Util.String.all_blank lines with
        | true -> []
        | false -> lines
      in
      { start_pad; tests = lines; end_pad }

let of_lines t =
  let { start_pad; tests; end_pad } = determine_padding t in
  let hpad = hpad_of_lines tests in
  let lines = unpad hpad tests in
  let lexer_input =
    lines |> List.map ((Fun.flip String.append) "\n") |> String.concat
  in
  let lines = Lexer_cram.token (Lexing.from_string lexer_input) in
  Log.debug (fun l ->
      l "Cram.of_lines (pad=%d) %a" hpad Fmt.(Dump.list dump_line) lines);
  let mk command output ~exit:exit_code =
    { command; output = List.rev output; exit_code }
  in
  let rec command_cont acc = function
    | `Command_cont c :: t -> command_cont (c :: acc) t
    | `Command_last c :: t -> (List.rev (c :: acc), t)
    | _ -> Fmt.failwith "invalid multi-line command"
  in
  let rec aux command output acc = function
    | [] when command = [] -> List.rev acc
    | [] -> List.rev (mk command output ~exit:0 :: acc)
    | `Exit exit :: t -> aux [] [] (mk command output ~exit :: acc) t
    | (`Ellipsis as o) :: t -> aux command (o :: output) acc t
    | `Command cmd :: t ->
        if command = [] then aux [ cmd ] [] acc t
        else aux [ cmd ] [] (mk command output ~exit:0 :: acc) t
    | `Command_first cmd :: t ->
        let cmd, t = command_cont [ cmd ] t in
        aux cmd [] (mk command output ~exit:0 :: acc) t
    | (`Output _ as o) :: t -> aux command (o :: output) acc t
    | (`Command_last s | `Command_cont s) :: t ->
        aux command output acc (`Output s :: t)
  in
  let hpad, tests =
    match lines with
    | `Command_first cmd :: t ->
        let cmd, t = command_cont [ cmd ] t in
        (hpad, aux cmd [] [] t)
    | `Command cmd :: t -> (hpad, aux [ cmd ] [] [] t)
    | [] -> (0, [])
    | `Output line :: _ ->
        if String.length line > 0 && line.[0] = '$' then
          failwith
            "Blocks must start with a command or similar, not with an output \
             line. To indicate a line as a command, start it with a dollar \
             followed by a space."
        else
          failwith
            "Blocks must start with a command or similar, not with an output \
             line. Please, make sure that there's no spare empty line, \
             particularly between the output and its input."
    | _ -> Fmt.failwith "invalid cram block: %a" Fmt.(Dump.list dump_line) lines
  in
  { start_pad; hpad; tests; end_pad }

let exit_code t = t.exit_code

(* http://tldp.org/LDP/abs/html/here-docs.html *)
let use_heredoc (t : t) = String.cut (List.hd t.command) ~sep:"<<" <> None

let command_line t =
  if not (use_heredoc t) then String.concat ~sep:" " t.command
  else String.concat ~sep:"\n" t.command
