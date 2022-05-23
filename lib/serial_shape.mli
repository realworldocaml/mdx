(** Helpers to describe the OCaml shape of data and serialize/deserialize
    it into different formats. *)

module Conv : sig
  type ('repr, 'true_type) t
  (** Type of converters from representation types to the true type.
    The representation type is the simplified type used for serialization
    while the true type is the type of the value as we'd like to use it
    in OCaml code.
    For instance a set is usually converted to a list before serializing
    it. In that scenario, the list is the representatio type and the set
    the true type. *)

  val make :
    from_repr:('repr -> ('true_type, Rresult.R.msg) result) ->
    to_repr:('true_type -> 'repr) ->
    ?equal:('true_type -> 'true_type -> bool) ->
    ?pp:'true_type Fmt.t ->
    unit ->
    ('repr, 'true_type) t
  (** Build a converter out of conversion functions between the repr
      and true type.
      [equal] and [pp] are optional and mostly used for testing of this
      module. *)
end

type 'a t
(** Type for generic data shape *)

val bool : bool t
val string : string t
val list : 'a t -> 'a list t
val pair : 'a t -> 'b t -> ('a * 'b) t
val conv : ('repr, 'true_type) Conv.t -> 'repr t -> 'true_type t

val choice3 : 'a t -> 'b t -> 'c t -> [ `C1 of 'a | `C2 of 'b | `C3 of 'c ] t
(** [choice3 s s' s''] allows for any of [s], [s'] or [s''].
    In case of ambiguity (for instance pairs and lists can be ambiguous when
    converting from opam values), the priority order is the order of
    arguments. In other words if the data could be interpreted both as
    [s] and [s'], it will be interpreted as [s].
    Note that since cmdliner arguments have no particular structure
    and everything is encoded as strings, you should always make string
    the lowest priority. *)

val from_opam_val :
  'a t -> OpamParserTypes.FullPos.value -> ('a, Rresult.R.msg) result
(** Deserialize from opam values *)

val to_opam_val : 'a t -> 'a -> OpamParserTypes.FullPos.value
(** Serialize into opam values *)

val cmdliner_conv : 'a t -> 'a Cmdliner.Arg.conv
(** Derive a Cmliner converter from a shape *)

(**/**)

(** Undocumented. Exposed for testing only *)

val equal : 'a t -> 'a -> 'a -> bool
val pp : 'a t -> 'a Fmt.t

(**/**)
