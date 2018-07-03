(** {1 Output} *)

type t = [`Output of string | `Ellipsis]
(** The type for test outputs. *)

val equal: t list -> t list -> bool
(** compare outputs. *)

val pp: ?pad:int -> t Fmt.t
val dump: t Fmt.t
