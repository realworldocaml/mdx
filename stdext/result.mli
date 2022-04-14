include module type of struct
  include Stdlib.Result
end

val bind : f:('a -> ('b, 'err) t) -> ('a, 'err) t -> ('b, 'err) t
val map : f:('a -> 'b) -> ('a, 'err) t -> ('b, 'err) t
val of_option : 'ok option -> error:'err -> ('ok, 'err) t

module O : sig
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( >>| ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( let+ ) : ('a, 'err) t -> ('a -> 'b) -> ('b, 'err) t
end

val map_error : f:('a -> 'b) -> ('ok, 'a) t -> ('ok, 'b) t

module List : sig
  val map : f:('a -> ('b, 'err) t) -> 'a list -> ('b list, 'err) t
  val iter : f:('a -> (unit, 'err) t) -> 'a list -> (unit, 'err) t
  val all : ('a, 'error) t list -> ('a list, 'error) t

  val fold_left :
    'a list -> f:('acc -> 'a -> ('acc, 'c) t) -> init:'acc -> ('acc, 'c) t

  val exists : 'a list -> f:('a -> (bool, 'err) t) -> (bool, 'err) t
  (** Same as [List.exists] with a predicate that can return an error.
      Returns [Ok true] if there is at least one element in the list
      that satisfies the predicate [f].
      Returns [Ok false] on empty lists.
      Returns [Error _] immediatly if the predicate does. *)
end
