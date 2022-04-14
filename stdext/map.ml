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

module Make (Key : Key) : S with type key = Key.t = struct
  include MoreLabels.Map.Make (Key)

  let mem t k = mem k t
  let find key t = find_opt t key
  let update t k ~f = update ~key:k ~f t
  let set t k v = add ~key:k ~data:v t
  let foldi t ~init ~f = fold t ~init ~f:(fun ~key ~data acc -> f key data acc)
  let values t = foldi t ~init:[] ~f:(fun _ v l -> v :: l) |> List.rev
  let keys t = foldi t ~init:[] ~f:(fun k _ l -> k :: l) |> List.rev

  let of_list =
    let rec loop acc = function
      | [] -> Result.Ok acc
      | (k, v) :: l -> (
          match find acc k with
          | None -> loop (set acc k v) l
          | Some v_old -> Error (k, v_old, v))
    in
    fun l -> loop empty l

  let of_list_map =
    let rec loop f acc = function
      | [] -> Result.Ok acc
      | x :: l ->
          let k, v = f x in
          if not (mem acc k) then loop f (set acc k v) l else Error k
    in
    fun l ~f ->
      match loop f empty l with
      | Result.Ok _ as x -> x
      | Error k -> (
          match
            List.filter l ~f:(fun x ->
                match Key.compare (fst (f x)) k with 0 -> true | _ -> false)
          with
          | x :: y :: _ -> Error (k, x, y)
          | _ -> assert false)

  let of_list_map_exn t ~f =
    match of_list_map t ~f with
    | Result.Ok x -> x
    | Error (_, _, _) -> invalid_arg "Map.of_list_map_exn"

  let of_list_exn l =
    match of_list l with
    | Result.Ok x -> x
    | Error (_, _, _) -> invalid_arg "Map.of_list_exn"
end
