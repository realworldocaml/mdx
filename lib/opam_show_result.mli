type t

(** [make lines] builds an Opam_show_result.t from an opam show output.
    The first field for each package must be 'name'*)
val make : string list -> (t, [>Rresult.R.msg]) result
val from_list : (string * string * string) list -> t
val get : package: string -> field: string -> t -> string option

val equal : t -> t -> bool
val pp : Format.formatter -> t -> unit
