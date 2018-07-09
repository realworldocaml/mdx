(** The type for block values. *)
type value = S.block_value

(** The type for supported code blocks. *)
type t = S.block

val dump: t Fmt.t

val pp_header: t Fmt.t
val pp_contents: t Fmt.t
val pp_footer: unit Fmt.t

val pp: t Fmt.t

val mode: t -> [`Non_det of [`Command|`Output] | `Normal]

val value: t -> value
val section: t -> (int * string) option
val header: t -> string option

val is_raw_ocaml: t -> bool
val executable_contents: t -> string list
val eval: t -> t
