type t

val make : string list -> (t, [> Rresult.R.msg ]) result
(** [make lines] builds an Opam_show_result.t from an opam show output.
    The first field for each package must be 'name'*)

val from_list : (string * string * string) list -> t
(** [from_list bindings] builds an Opam_show_result.t from a list of
    bindings (package, field, value). *)

val get : package:string -> field:string -> t -> string option
(** [get ~package ~field t] retrieves a field from a package. Returns
    None if the package or the field doesn't exist. *)

val equal : t -> t -> bool
(** Equality test on Opam_show_result.t *)

val pp : Format.formatter -> t -> unit
(** Pretty-printer *)
