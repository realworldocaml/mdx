module List : sig
  val map : f: ('a -> ('b, 'err) result) -> 'a list -> ('b list, 'err) result
end
