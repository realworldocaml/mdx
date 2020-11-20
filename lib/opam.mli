module Dev_repo : sig
  type vcs = Git | Other of string

  val equal_vcs : vcs -> vcs -> bool

  val pp_vcs : vcs Fmt.t

  type t = { vcs : vcs option; uri : Uri.t }

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_string : string -> t
end

module Url : sig
  type t = Git of { repo : string; ref : string option } | Other of string

  (* This includes archives, other VCS and rsync opam src URLs *)

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_opam : OpamFile.URL.t -> t
end

module Package_summary : sig
  type t = {
    name : string;
    version : string;
    url_src : Url.t option;
    dev_repo : string option;
    uses_dune : bool;
  }

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_opam : pkg:OpamPackage.t -> OpamFile.OPAM.t -> t

  val is_virtual : t -> bool
  (** A package is considered virtual if it has no url.src or no dev-repo. *)

  val is_base_package : t -> bool

  val uses_dune : t -> bool
end

module Pp : sig
  val package : OpamPackage.t Fmt.t
end

val depends_on_dune : OpamTypes.filtered_formula -> bool
(** Returns whether the given depends field formula contains a dependency to dune or jbuilder *)

val tag_from_archive : string -> string option
(** Infers the git tag or ref from the url.src field *)

val classify_package :
  package:Types.Opam.package ->
  dev_repo:string option ->
  archive:string option ->
  unit ->
  Types.Opam.repo * string option
(** Returns the repo and tag from the given package, dev-repo and url.src fields *)

val pull_tree :
  url:OpamUrl.t ->
  dir:Fpath.t ->
  OpamStateTypes.unlocked OpamStateTypes.global_state ->
  (unit, [ `Msg of string ]) result
(** Pulls the sources from [url] to [dir] using opam's library.
    This benefits from opam's global cache *)
