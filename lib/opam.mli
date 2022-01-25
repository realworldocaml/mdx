module Url : sig
  type t = Git of { repo : string; ref : string option } | Other of string

  (* This includes archives, other VCS and rsync opam src URLs *)

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_opam_field : OpamFile.URL.t -> t

  val from_opam : OpamUrl.t -> t

  val is_local_filesystem : OpamUrl.t -> bool
  (** Returns whether the URL points to a non version controlled local folder or
      file *)
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

module Extra_field : sig
  (** Module for parsing and printing of opam file extensions
      specific to opam-monorepo *)

  type 'a t
  (** Type of extra opam field holding values of type ['a]. *)

  val make :
    name:string ->
    to_opam_value:('a -> OpamParserTypes.FullPos.value) ->
    from_opam_value:
      (OpamParserTypes.FullPos.value -> ('a, [ `Msg of string ]) result) ->
    'a t
  (** [make ~name ~to_opam_value ~from_opam_value] returns an extra field
      which is named ["x-opam-monorepo-<name>"] and that is converted to and
      from generic opam values using the provided functions. *)

  val name : _ t -> string
  (** Return the full name of the field *)

  val set : 'a t -> 'a -> OpamFile.OPAM.t -> OpamFile.OPAM.t
  (** Sets the field in the given opam file, potentially overwriting the
      previous value. *)

  val get : 'a t -> OpamFile.OPAM.t -> ('a option, [ `Msg of string ]) result
  (** Returns the value of the given extra field in the given opam file if
      the extra field is set. *)
end

module Pos : sig
  val default : OpamParserTypes.FullPos.pos

  val with_default : 'a -> 'a OpamParserTypes.FullPos.with_pos

  val errorf :
    pos:OpamParserTypes.FullPos.pos ->
    ('a, unit, string, ('b, [> `Msg of string ]) result) format4 ->
    'a

  val value_errorf :
    value:OpamParserTypes.FullPos.value ->
    ('a, unit, string, ('b, [> `Msg of string ]) result) format4 ->
    'a

  val unexpected_value_error :
    expected:string ->
    OpamParserTypes.FullPos.value ->
    (_, [> `Msg of string ]) result
end

module Value : sig
  (** Utilities to convert base opam values to/from ocaml *)

  module String : sig
    val to_value : string -> OpamParserTypes.FullPos.value

    val from_value :
      OpamParserTypes.FullPos.value -> (string, Rresult.R.msg) result
  end

  module List : sig
    val to_value :
      ('a -> OpamParserTypes.FullPos.value) ->
      'a list ->
      OpamParserTypes.FullPos.value

    val from_value :
      (OpamParserTypes.FullPos.value -> ('a, Rresult.R.msg) result) ->
      OpamParserTypes.FullPos.value ->
      ('a list, Rresult.R.msg) result
  end
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
