open! Import

type t = OpamPackage.t * OpamUrl.t

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit

val sort_uniq : t list -> (t list, [> `Msg of string ]) result
(** Sorts and deduplicate all the combined pin-depends of a repository.
    Returns an error if the same package is pinned twice to different URLs or versions. *)

val group_by_url : t list -> OpamPackage.t list OpamUrl.Map.t
