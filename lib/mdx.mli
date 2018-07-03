(** [Mdx] is a library to manipulate markdown code blocks. *)

module Output = Output
module Cram = Cram
module Toplevel = Toplevel
module Block = Block

(** {1 Raw lexemes} *)

(** The type for the lines of a markdown file. *)
type line =
  | Section of (int * string)
  | Text    of string
  | Block   of Block.t

val pp_line: line Fmt.t
(** [pp_line] is the pretty-printer for markdown lines. *)

type t = line list
(** The type for mdx documents. *)

val pp: t Fmt.t
(** [pp] is the pretty printer for mdx documents. Should be idempotent
   with {!of_string}. *)

val to_string: t -> string

val of_string: string -> t

val parse_file: string ->  t

val parse_lexbuf: Lexing.lexbuf -> t

val run: f:(string -> t -> string) -> string -> unit
(** [run ~f n] runs the expect callback [f] over the file named
   [n]. [f] is called with the raw contents of [n] and its structured
   contents; it returns the new file contents. If the result of [f] is
   different from the initial contents, then [$n.corrected] is created
   with the new contents. *)

val section_of_line: line -> (int * string) option
(** [section_of_line l] is [l]'s section. *)

val filter_section: Re.re -> t -> t option
(** [section re t] is the subset of [t] such that their section
   matches with [re]. *)
