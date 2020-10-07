val map_error : f:('a -> 'b) -> ('ok, 'a) result -> ('ok, 'b) result

module List : sig
  val map : f:('a -> ('b, 'err) result) -> 'a list -> ('b list, 'err) result
end
