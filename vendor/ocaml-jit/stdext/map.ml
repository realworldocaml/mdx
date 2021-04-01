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

module Make (Key : Key) : S with type key = Key.t = struct
  include MoreLabels.Map.Make (Key)

  module Bindings = struct
    type 'a t = (Key.t * 'a) list [@@deriving show]
  end

  let pp pp_a fmt t = Bindings.pp pp_a fmt (bindings t)

  let show show_a t = Bindings.show show_a (bindings t)
end
