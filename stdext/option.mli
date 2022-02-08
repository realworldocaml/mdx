include module type of struct
  include Stdlib.Option
end

module O : sig
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

val value : default:'a -> 'a t -> 'a

val bind : f:('a -> 'b t) -> 'a t -> 'b t

val map : f:('a -> 'b) -> 'a t -> 'b t

val map_default : f:('a -> 'b) -> default:'b -> 'a t -> 'b
