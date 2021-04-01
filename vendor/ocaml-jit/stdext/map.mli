module type S = sig
  include MoreLabels.Map.S

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
end

module type Key = sig
  include MoreLabels.Map.OrderedType

  val pp : Format.formatter -> t -> unit

  val show : t -> string
end

module Make (Key : Key) : S with type key = Key.t
