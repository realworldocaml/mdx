type t = Markdown | Cram | Mli | Mld

val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
val infer : file:string -> t option
val of_string : string -> t option
