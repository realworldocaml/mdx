(** Location of blocks within the source file. *)

type t = { fname : string; line : int; column : int }

val none : t
val pp : Format.formatter -> t -> unit

val to_lexpos : ?offset:int -> t -> Lexing.position
(** Turn into a lexing position, for example, to start a new lexer. [offset]
      sets the position in the input to start lexing, normally [0]. *)

val of_lexpos : Lexing.position -> t
(** While parsing, this should be the [loc_start] field. *)
