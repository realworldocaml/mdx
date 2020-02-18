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

(** Code blocks headers. *)

module Header : sig
  type t = Shell | OCaml | Other of string

  val pp : Format.formatter -> t -> unit

  val of_string : string -> t option
end

(** Code blocks. *)

type cram_value = {
  pad : int;
  tests : Cram.t list;
  non_det : Label.non_det option;
}

type ocaml_value = {
  env : string;
  (** [env] is the name given to the environment where tests are run. *)
  non_det : Label.non_det option;
}

type toplevel_value = {
  phrases : Toplevel.t list;
  env : string;
  (** [env] is the name given to the environment where tests are run. *)
  non_det : Label.non_det option;
}

type include_value = {
  file_included : string;
  (** [file_included] is the name of the file to synchronize with. *)
  part_included : string option;
  (** [part_included] is the part of the file to synchronize with.
      If lines is not specified synchronize the whole file. *)
}

(** The type for block values. *)
type value =
  | Raw
  | OCaml of ocaml_value
  | Error of string list
  | Cram of cram_value
  | Toplevel of toplevel_value
  | Include of include_value

type section = int * string
(** The type for sections. *)

(** The type for supported code blocks. *)
type t = {
  line : int;
  file : string;
  section : section option;
  dir : string option;
  source_trees : string list;
  required_packages : string list;
  labels : Label.t list;
  header : Header.t option;
  contents : string list;
  skip : bool;
  version : (Label.Relation.t * Ocaml_version.t) option;
  value : value;
}

val mk:
  line:int
  -> file:string
  -> section:section option
  -> labels:Label.t list
  -> header:Header.t option
  -> contents:string list
  -> (t, [`Msg of string]) Result.result

(** {2 Printers} *)

val dump : t Fmt.t
(** [dump] is the printer for dumping code blocks. Useful for debugging. *)

val pp_header : ?syntax:Syntax.t -> t Fmt.t
(** [pp_header] pretty-prints full block headers with the labels. *)

val pp_contents : ?syntax:Syntax.t -> t Fmt.t
(** [pp_contents] pretty-prints block contents. *)

val pp_footer : ?syntax:Syntax.t -> unit Fmt.t
(** [pp_footer] pretty-prints block footer. *)

val pp : ?syntax:Syntax.t -> t Fmt.t
(** [pp] pretty-prints blocks. *)

val pp_line_directive : (string * int) Fmt.t
(** [pp_line_directive] pretty-prints a line directive given as a
   filename and line number. *)

(** {2 Accessors} *)

val line: t -> int

val filename : t -> string

val section : t -> section option
(** [section t] is [t]'s section. *)

val contents : t -> string list

val value : t -> value
(** [value t] is [t]'s value. *)

val non_det : t -> Label.non_det option
(** Whether a block's command or output is non-deterministic. *)

val directory : t -> string option
(** [directory t] is the directory where [t] tests should be run. *)

val source_trees : t -> string list
(** [source_trees t] is the list of extra source-trees to add as
   dependency of the code-block. *)

val file : t -> string option
(** [file t] is the name of the file to synchronize [t] with. *)

val environment : t -> string
(** [environment t] is the name given to the environment where [t] tests
    are run. *)

val set_variables : t -> (string * string) list
(** [set_variable t] is the list of environment variable to set and their values *)

val unset_variables : t -> string list
(** [unset_variable t] is the list of environment variable to unset *)

val explicit_required_packages : t -> string list
(** [explicit_required_packages t] returns the list of packages explicitly required by the user
    through require-package labels in the block [t]. *)

val required_libraries : t -> (Library.Set.t, string) Result.result
(** [required_libraries t] returns the set of libaries that are loaded through [#require]
    statements in the block [t]. Always returns an empty set if [t] isn't a toplevel
    block. *)

val skip : t -> bool
(** [skip t] is true iff [skip] is in the labels of [t]. *)

val header : t -> Header.t option
(** [header t] is [t]'s header. *)

val executable_contents : t -> string list
(** [executable_contents t] is either [t]'s contents if [t] is a raw
   or a cram block, or [t]'s commands if [t] is a toplevel fragments
   (e.g. the phrase result is discarded). *)

val is_active : ?section:string -> t -> bool

(** {2 Parsers} *)

val require_from_line : string -> (Library.Set.t, string) Result.result
(** [require_from_line line] returns the set of libraries imported by the
    #require statement on [line] or an empty set if [line] is not a require
    statement. *)

val require_from_lines : string list -> (Library.Set.t, string) Result.result
(** Same as [require_from_line] but aggregated over several lines *)
