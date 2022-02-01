(** Utilities for extracting configuraion from target packages opam
    files available in the project sources in opam extensions *)

open Import

type t = {
  global_vars : OpamVariable.variable_contents String.Map.t option;
  repositories : OpamUrl.Set.t option;
}
(** Type for solver configuration bits encoded in opam extensions
    of target packages opam files.
    [@repositories] is the explicit list of opam-repositories URLs to use
    for opam metadata
    [@global_vars] allows the user to define the opam variables to be used by
    the solver when running in reproducible mode. *)

val get :
  opam_monorepo_cwd:Fpath.t -> OpamFile.OPAM.t -> (t, Rresult.R.msg) result
(** Parses the config from the opam extensions in the given opam file.
    If the extensions are missing, the corresponding field is set to [None].
    [opam_monorepo_cwd] is the absolute path from which opam monorepo was
    invoked. It is used to rewrite local FS URLS (["file://"]) as these have
    to be absolute. This makes it possible to refer to repositories defined
    locally, in the project. *)

val set : opam_monorepo_cwd:Fpath.t -> t -> OpamFile.OPAM.t -> OpamFile.OPAM.t
(** Writes the given config into the extensions of the given opam file.
    If a config field is [None] the corresponding field won't be added
    to the opam file.
    [opam_monorepo_cwd] is the absolute path from which opam monorepo was
    invoked. It is used to rewrite local FS URLS (["file://"]) as these have
    to be absolute. This makes it possible to refer to repositories defined
    locally, in the project. *)

val merge : t list -> (t, Rresult.R.msg) result
(** Merges config from different opam files into a single, shared config. *)

(**/**)

(** Undocumented *)

module Private : sig
  (** Private API module used to expose functions for testing purposes.
      DO NOT USE! *)
  module Opam_repositories : sig
    val from_opam_value :
      opam_monorepo_cwd:string ->
      OpamParserTypes.FullPos.value ->
      (OpamUrl.Set.t, [ `Msg of string ]) result

    val to_opam_value :
      opam_monorepo_cwd:string -> OpamUrl.Set.t -> OpamParserTypes.FullPos.value
  end

  module Opam_global_vars : sig
    val from_opam_value :
      OpamParserTypes.FullPos.value ->
      (OpamVariable.variable_contents String.Map.t, [ `Msg of string ]) result

    val to_opam_value :
      OpamVariable.variable_contents String.Map.t ->
      OpamParserTypes.FullPos.value
  end
end

(**/**)
