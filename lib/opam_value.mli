(* Convert a sexpression to an opam value using only lists and strings *)
val from_sexp : ?pos: OpamTypes.pos -> Sexplib0.Sexp.t -> OpamTypes.value

(* Converts an opam value to a sexpression assuming it's only made of
   opam lists and strings *)
val to_sexp_strict : OpamTypes.value -> (Sexplib0.Sexp.t, [> `Msg of string]) result
