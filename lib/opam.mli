module Dev_repo : sig
  type vcs = Git | Other of string

  val equal_vcs : vcs -> vcs -> bool

  val pp_vcs : vcs Fmt.t

  type t = { vcs : vcs option; uri : Uri.t }

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_string : string -> t
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
