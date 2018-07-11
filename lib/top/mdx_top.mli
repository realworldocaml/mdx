val run:
  verbose:bool ref ->
  silent:bool ref ->
  ?verbose_findlib:bool ->
  string list -> string list

val init: unit -> unit
val verbose: bool ref -> unit
val silent: bool ref -> unit
