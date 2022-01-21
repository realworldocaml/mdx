(** Utilities for extracting information from target packages opam
    metadata such as opam extensions used by opam-monorepo *)

open Import

type config = {
  global_vars : OpamVariable.variable_contents String.Map.t option;
  repositories : OpamUrl.Set.t option;
}
(** Type for solver configuration bits encoded in opam extensions
    of target packages opam files.
    [@repositories] is the explicit list of opam-repositories URLs to use
    for opam metadata
    [@global_vars] allows the user to define the opam variables to be used by
    the solver when running in reproducible mode. *)

val extract_config :
  opam_monorepo_cwd:string -> OpamFile.OPAM.t -> (config, Rresult.R.msg) result
(** Parses the config from the opam extensions in the given opam file.
    If the extensions are missing, the corresponding field is set to [None].
    [opam_monorepo_cwd] is the absolute path from which opam monorepo was
    invoked. It is used to rewrite local FS URLS (["file://"]) as these have
    to be absolute. This makes it possible to refer to repositories defined
    locally, in the project. *)

val set_config :
  opam_monorepo_cwd:string -> config -> OpamFile.OPAM.t -> OpamFile.OPAM.t
(** Writes the given config into the extensions of the given opam file.
    If a config field is [None] the corresponding field won't be added
    to the opam file.
    [opam_monorepo_cwd] is the absolute path from which opam monorepo was
    invoked. It is used to rewrite local FS URLS (["file://"]) as these have
    to be absolute. This makes it possible to refer to repositories defined
    locally, in the project. *)

val merge_config : config list -> (config, Rresult.R.msg) result
(** Merges config from different opam files into a single, shared config. *)

val opam_monorepo_cwd_from_root : Fpath.t -> string
