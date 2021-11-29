module Url : sig
  type t = Git of { repo : string; ref : string option } | Other of string

  (* This includes archives, other VCS and rsync opam src URLs *)

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_opam_field : OpamFile.URL.t -> t

  val from_opam : OpamUrl.t -> t
end

module Package_summary : sig
  type t = {
    package : OpamPackage.t;
    url_src : Url.t option;
    hashes : OpamHash.t list;
    dev_repo : string option;
    depexts : (OpamSysPkg.Set.t * OpamTypes.filter) list;
  }

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_opam : pkg:OpamPackage.t -> OpamFile.OPAM.t -> t

  val is_virtual : t -> bool
  (** A package is considered virtual if it has no url.src or no dev-repo. *)

  val is_base_package : t -> bool
end

module Hash : sig
  val equal : OpamHash.t -> OpamHash.t -> bool
end

module Pp : sig
  val package : OpamPackage.t Fmt.t

  val raw_package : OpamPackage.t Fmt.t

  val package_name : OpamPackage.Name.t Fmt.t

  val version : OpamPackage.Version.t Fmt.t

  val hash : OpamHash.t Fmt.t

  val url : OpamUrl.t Fmt.t
end

val depends_on_dune : allow_jbuilder:bool -> OpamTypes.filtered_formula -> bool
(** Returns whether the given depends field formula contains a dependency to dune or jbuilder *)

val depends_on_compiler_variants : OpamTypes.filtered_formula -> bool
(** Returns whether the given depends field formula contains a dependency
    towards a compiler variant (such as a compiler with flambda or afl enabled for instance).
    This is detected by looking direct dependencies on ocaml-variants or dependencies on
    any relevant ocaml-option-* packages. *)

val pull_tree :
  url:OpamUrl.t ->
  hashes:OpamHash.t list ->
  dir:Fpath.t ->
  OpamStateTypes.unlocked OpamStateTypes.global_state ->
  (unit, [> `Msg of string ]) result OpamProcess.job
(** Pulls the sources from [url] to [dir] using opam's library. Returns the target directory path
    if sucessful and an error otherwise.
    This benefits from opam's global cache.*)

val local_package_version :
  OpamFile.OPAM.t ->
  explicit_version:OpamTypes.version option ->
  OpamTypes.version
(** Determine the version for a local package.
    - if [explicit_version] is passed, use it
    - if the opam file has a version field, use it
    - otherwise, use a default value (["zdev"])
    *)
