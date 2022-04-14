include module type of struct
  include ListLabels
end

val is_empty : 'a t -> bool
val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
val find_map : 'a list -> f:('a -> 'b option) -> 'b option
val partition_map : 'a t -> f:('a -> ('b, 'c) Either.t) -> 'b t * 'c t
val filter_opt : 'a option t -> 'a t
val concat_map : f:('a -> 'b list) -> 'a list -> 'b list

val max_exn : compare:('a -> 'a -> int) -> 'a list -> 'a
(** Returns the greatest element in the list according to [compare].
    Raises Invalig_argument on empty lists *)

val compare : compare:('a -> 'a -> int) -> 'a list -> 'a list -> int
