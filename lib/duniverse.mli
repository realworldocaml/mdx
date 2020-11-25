module O = Opam

type unresolved = Git.Ref.t

type resolved = Git.Ref.resolved

module Opam : sig
  type t = { name : string; version : string }
  (** Type of dependencies to install through opam *)

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val to_opam : t -> OpamPackage.t

  val from_opam : OpamPackage.t -> t
end

module Repo : sig
  module Url : sig
    type 'ref t = Git of { repo : string; ref : 'ref } | Other of string

    val equal : ('ref -> 'ref -> bool) -> 'ref t -> 'ref t -> bool

    val pp : 'ref Fmt.t -> 'ref t Fmt.t

    val to_string : resolved t -> string

    val to_opam_url : resolved t -> OpamUrl.t

    val from_opam_url : OpamUrl.t -> (resolved t, [ `Msg of string ]) result
    (** Converts an [OpamUrl.t] to a resolved URL. Assumes the ref after the "#" is
        a commit hash. Returns an error on git URLs with no such ref. *)
  end

  type 'ref t = { dir : string; url : 'ref Url.t; provided_packages : Opam.t list }
  (** Type of dependencies to clone in the duniverse *)

  val equal : ('ref -> 'ref -> bool) -> 'ref t -> 'ref t -> bool

  (**/**)

  (* Exposed for test purposes only *)

  val pp : 'ref Fmt.t -> 'ref t Fmt.t

  module Package : sig
    type t = { opam : Opam.t; dev_repo : string; url : unresolved Url.t }

    val equal : t -> t -> bool

    val pp : t Fmt.t

    val from_package_summary :
      get_default_branch:(string -> (string, Rresult.R.msg) result) ->
      O.Package_summary.t ->
      (t option, [ `Msg of string ]) result
  end

  val from_packages : dev_repo:Dev_repo.t -> Package.t list -> unresolved t

  (**/**)
end

type t = resolved Repo.t list
(** The type of dependencies to be pulled into the duniverse *)

val equal : t -> t -> bool

val from_package_summaries :
  get_default_branch:(string -> (string, Rresult.R.msg) result) ->
  O.Package_summary.t list ->
  (unresolved Repo.t list, [ `Msg of string ]) result
(** Build opamverse and duniverse from a list of [Types.Opam.entry] values.
    It filters out virtual packages and packages with unknown dev-repo.  *)

val resolve :
  resolve_ref:(repo:string -> ref:unresolved -> (resolved, Rresult.R.msg) result) ->
  unresolved Repo.t list ->
  (t, Rresult.R.msg) result
(** Apply the given [resolve_ref] function to bind each source repo to a specific commit
    rather than a "floating" ref. *)

(**/**)

(* Exposed for test purposes only *)

val pp : t Fmt.t

(**/**)
