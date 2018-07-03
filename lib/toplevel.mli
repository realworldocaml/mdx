type t = S.toplevel

val dump: t Fmt.t
val pp: ?pad:int -> t Fmt.t
val pp_command: ?pad:int -> t Fmt.t

val of_lines: string list -> int * t list
val command: t -> string list
val output: t -> S.output list
