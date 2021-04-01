include module type of struct
  include Stdlib.Option
end

module Op : sig
  val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option

  val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
end
