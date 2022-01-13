(** Utilities for extracting information from target packages opam
    metadata such as opam extensions used by opam-monorepo *)

open Import

module type EXTENSION = sig
  (** The type of modules used to extract data stored in opam extensions *)

  type t
  (** The type of the data encoded in the extension *)

  val field : t Opam.Extra_field.t

  val merge : t list -> (t, Rresult.R.msg) result
  (** Function to merge the extensions into a single one when it appears
    in the metadata of several target packages *)
end

module Opam_repositories : EXTENSION with type t = OpamUrl.Set.t
(** Opam extension used to specify an explicit list of opam-repositories
    that the solver must use when generating the lockfile to make it
    reproducible. *)

module Opam_global_vars :
  EXTENSION with type t = OpamVariable.variable_contents String.Map.t
(** Opam extension used to specify the set of opam global variables to
    use when generating a reproducible lockfile. *)

type config = {
  global_vars : Opam_global_vars.t option;
  repositories : Opam_repositories.t option;
}

val extract_config : OpamFile.OPAM.t -> (config, Rresult.R.msg) result

val merge_config : config list -> (config, Rresult.R.msg) result
