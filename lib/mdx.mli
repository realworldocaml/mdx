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

(** [Mdx] is a library to manipulate markdown code blocks.

    [mdx] allows to execute code blocks inside markdown files. The
   supported code {{!Block}blocks} are either {{!Cram}cram-like}
   tests, raw OCaml fragments or {{!Toplevel}toplevel} phrases.

    Cram tests and toplevel phrases are sequences of commands and
   {{!Output}outputs}.  *)

module Lexer = Lexer
module Output = Output
module Cram = Cram
module Toplevel = Toplevel
module Library = Library
module Block = Block
module Migrate_ast = Migrate_ast
module Compat = Compat
module Util = Util
module Prelude = Prelude
module Syntax = Syntax

(** {2 Lines} *)

type syntax = Syntax.t =
  | Normal
  | Cram
  | Mli

(** The type for the lines of a markdown or cram file. *)
type line =
  | Section of (int * string)
  | Text    of string
  | Block   of Block.t

val pp_line: ?syntax:syntax -> line Fmt.t
(** [pp_line] is the pretty-printer for markdown or cram lines. *)

(** {2 Document} *)

type t = line list
(** The type for mdx documents. *)

val pp: ?syntax:syntax -> t Fmt.t
(** [pp] is the pretty printer for mdx documents. Should be idempotent
   with {!of_string}. *)

val to_string: t -> string
(** [to_string t] converts the document [t] to a string. *)

val of_string: syntax -> string -> t
(** [of_string syntax s] is the document [t] such that
    [to_string ~syntax t = s]. *)

val parse_file: syntax -> string ->  t
(** [parse_file s] is {!of_string} of [s]'s contents. *)

val parse_lexbuf: string -> syntax -> Lexing.lexbuf -> t
(** [parse_lexbuf l] is {!of_string} of [l]'s contents. *)

val parse_mli: string -> t
(** Slice an mli file into its [Text] and [Block] parts. *)

(** {2 Evaluation} *)

val run_to_stdout : ?syntax: syntax -> f:(string -> t -> string) -> string -> unit
(** [run_to_stdout ?syntax ~f file] runs the callback [f] on the raw and
    structured content of [file], as specified  by [syntax] (defaults to [Normal]).
    The returned corrected version is then written to stdout. *)

val run_to_file :
  ?syntax: syntax ->
  f:(string -> t -> string) ->
  outfile: string ->
  string ->
  unit
(** Same as [run_to_stdout] but writes the corrected version to [outfile]*)

val run: ?syntax:syntax -> ?force_output:bool -> f:(string -> t -> string) -> string -> unit
(** [run_to_file ?syntax ?force_output ~f ~outfile file] runs the callback [f]
    similarly to [run_to_stdout] to generate its corrected version. If
    [force_output] is [true] (defaults to [false]) or if the corrected version
    differs from the original file content, it writes it to <file>.corrected.
    Otherwise, if <file>.corrected already exists, it removes it. *)

(** {2 Filtering} *)

val section_of_line: line -> (int * string) option
(** [section_of_line l] is [l]'s section. *)

val filter_section: Re.re -> t -> t option
(** [section re t] is the subset of [t] such that their section
   matches with [re]. *)
