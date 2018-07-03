(** Cram tests *)

(** {1 Cram tests} *)

type t = S.cram
(** The type for cram tests. *)

val exit_code: t -> int
val use_heredoc: t -> bool
val command_line: t -> string

val of_lines: string list -> int * t list

(** {1 Pretty-printer} *)

val pp: ?pad:int -> t Fmt.t
(** [pp] is the pretty-printer for cram tests. *)

val dump: t Fmt.t

val pp_command: ?pad:int -> t Fmt.t
val pp_exit_code: ?pad:int -> int Fmt.t
