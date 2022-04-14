module type S = sig
  include MoreLabels.Map.S

  val mem : 'a t -> key -> bool
  val set : 'a t -> key -> 'a -> 'a t
  val find : 'a t -> key -> 'a option
  val update : 'a t -> key -> f:('a option -> 'a option) -> 'a t
  val values : 'a t -> 'a list
  val keys : 'a t -> key list
  val of_list : (key * 'a) list -> ('a t, key * 'a * 'a) Result.t
  val of_list_exn : (key * 'a) list -> 'a t
  val of_list_map_exn : 'a list -> f:('a -> key * 'b) -> 'b t
end

module type Key = sig
  include MoreLabels.Map.OrderedType
end

module Make (Key : Key) : S with type key = Key.t
