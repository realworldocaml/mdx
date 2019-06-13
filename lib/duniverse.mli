module Element : sig
  type t =
    | Opam of { name : string; version : string option [@default None] [@sexp_drop_default] }
    | Repo of
        { dir : string;
          upstream : string;
          ref : string [@default "master"] [@sexp_drop_default]
        }
  [@@deriving sexp]

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_opam_entry :
    get_default_branch:(string -> (string, Rresult.R.msg) result) ->
    Types.Opam.entry ->
    (t option, Rresult.R.msg) result

  val dedup_upstream : t list -> t list
  (** [dedup_upstream l] returns [l] with a single repo for any given upstream URL. *)
end

module Config : sig
  type t =
    { root_packages : Types.Opam.package list;
      excludes : Types.Opam.package list;
      pins : Types.Opam.pin list;
      remotes : string list; [@default []]
      branch : string [@default "master"]
    }
  [@@deriving sexp]
end

type t = {
  config : Config.t;
  content : Element.t list
}
[@@deriving sexp]

val load : file:Fpath.t -> (t, [> `Msg of string ]) result

val save : file:Fpath.t -> t -> (unit, [> `Msg of string ]) result
