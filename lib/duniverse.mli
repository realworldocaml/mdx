module Deps : sig
  module Opam : sig
    (** Type of dependencies to install through opam *)
    type t = { name : string; version : string option }

    val equal : t -> t -> bool

    val pp : t Fmt.t
  end

  module Source : sig
    (** Type of dependencies to clone in the duniverse *)
    type t = { dir : string; upstream : string; ref : string }

    val equal : t -> t -> bool

    (**/**)

    (* Exposed for test purposes only *)

    val raw_pp : t Fmt.t

    val aggregate : t -> t -> t

    val aggregate_list : t list -> t list

    (**/**)
  end

  (** The type for dependencies of a project. The duniverse and opamverse are complementary,
      that is a dependency either can be installed by pulling the sources and is in the duniverse
      or has to be installed through opam and is in the opamverse. *)
  type t = { opamverse : Opam.t list; duniverse : Source.t list }

  val equal : t -> t -> bool

  val from_opam_entries :
    get_default_branch:(string -> (string, Rresult.R.msg) result) ->
    Types.Opam.entry list ->
    (t, [ `Msg of string ]) result
  (** Build opamverse and duniverse from a list of [Types.Opam.entry] values.
      It filters out virtual packages and packages with unknown dev-repo.  *)

  val count : t -> int
  (** Returns the total number of dependencies represented by the given [t] value. *)

  (**/**)

  (* Exposed for test purposes only *)

  val raw_pp : t Fmt.t

  module One : sig
    type t = Opam of Opam.t | Source of Source.t

    val equal : t -> t -> bool

    val raw_pp : t Fmt.t

    val from_opam_entry :
      get_default_branch:(string -> (string, Rresult.R.msg) result) ->
      Types.Opam.entry ->
      (t option, [ `Msg of string ]) result
  end

  (**/**)
end

module Config : sig
  type t = {
    root_packages : Types.Opam.package list;
    excludes : Types.Opam.package list;
    pins : Types.Opam.pin list;
    remotes : string list; [@default []]
    branch : string [@default "master"]
  }
  [@@deriving sexp]
end

type t = { config : Config.t; deps : Deps.t } [@@deriving sexp]

val load : file:Fpath.t -> (t, [> `Msg of string ]) result

val save : file:Fpath.t -> t -> (unit, [> `Msg of string ]) result
