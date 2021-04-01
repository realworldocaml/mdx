include module type of struct
  include Stdlib.Result
end

module Op : sig
  val ( let* ) :
    ('a, 'err) result -> ('a -> ('b, 'err) result) -> ('b, 'err) result

  val ( let+ ) : ('a, 'err) result -> ('a -> 'b) -> ('b, 'err) result
end

module List : sig
  val map_all :
    f:('a -> ('b, 'err) result) -> 'a list -> ('b list, 'err list) result

  val iter_all :
    f:('a -> (unit, 'err) result) -> 'a list -> (unit, 'err list) result
end
