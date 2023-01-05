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

module Lexer_mdx = Lexer_mdx
module Output = Output
module Cram = Cram
module Deprecated = Deprecated
module Document = Document
module Toplevel = Toplevel
module Part = Part
module Block = Block
module Mli_parser = Mli_parser
module Compat = Compat
module Util = Util
module Prelude = Prelude
module Syntax = Syntax
module Label = Label
module Dep = Dep
module Ocaml_env = Ocaml_env
module Stable_printer = Stable_printer
include module type of Document

val dump : line list Fmt.t

(** {2 Document} *)

val of_string : Syntax.t -> string -> (t, [ `Msg of string ] list) result
(** [of_string syntax s] is the document [t] such that
    [to_string ~syntax t = s]. *)

val parse_file : Syntax.t -> string -> (t, [ `Msg of string ] list) result
(** [parse_file s] is {!of_string} of [s]'s contents. *)

(** {2 Evaluation} *)

val run_to_stdout :
  ?syntax:Syntax.t ->
  f:(string -> t -> string) ->
  string ->
  (unit, [ `Msg of string ] list) result
(** [run_to_stdout ?syntax ~f file] runs the callback [f] on the raw and
    structured content of [file], as specified  by [syntax] (defaults to [Markdown]).
    The returned corrected version is then written to stdout. *)

val run_to_file :
  ?syntax:Syntax.t ->
  f:(string -> t -> string) ->
  outfile:string ->
  string ->
  (unit, [ `Msg of string ] list) result
(** Same as [run_to_stdout] but writes the corrected version to [outfile]*)

val run :
  ?syntax:Syntax.t ->
  ?force_output:bool ->
  f:(string -> t -> string) ->
  string ->
  (unit, [ `Msg of string ] list) result
(** [run_to_file ?syntax ?force_output ~f ~outfile file] runs the callback [f]
    similarly to [run_to_stdout] to generate its corrected version. If
    [force_output] is [true] (defaults to [false]) or if the corrected version
    differs from the original file content, it writes it to <file>.corrected.
    Otherwise, if <file>.corrected already exists, it removes it. *)

(** {2 Filtering} *)

val section_of_line : line -> (int * string) option
(** [section_of_line l] is [l]'s section. *)

val filter_section : Re.re -> t -> t option
(** [section re t] is the subset of [t] such that their section
   matches with [re]. *)
