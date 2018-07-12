type t

val init: verbose:bool -> silent:bool -> verbose_findlib:bool -> unit -> t

val run: t -> string list -> string list
