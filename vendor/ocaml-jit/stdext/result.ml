include Stdlib.Result

module Op = struct
  let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

  let ( let+ ) r f = match r with Error e -> Error e | Ok x -> Ok (f x)
end

module List = struct
  let map_all ~f l =
    let rec aux acc l =
      match (acc, l) with
      | Error err_list, [] -> Error (List.rev err_list)
      | Ok mapped, [] -> Ok (List.rev mapped)
      | _, hd :: tl ->
          let acc =
            match (f hd, acc) with
            | Error err, Error err_list -> Error (err :: err_list)
            | Error err, _ -> Error [ err ]
            | Ok _, Error _ -> acc
            | Ok x, Ok mapped -> Ok (x :: mapped)
          in
          aux acc tl
    in
    aux (Ok []) l

  let iter_all ~f l =
    let rec aux acc l =
      match (acc, l) with
      | Error err_list, [] -> Error (List.rev err_list)
      | Ok _, [] -> acc
      | _, hd :: tl ->
          let acc =
            match (f hd, acc) with
            | Error err, Error err_list -> Error (err :: err_list)
            | Error err, _ -> Error [ err ]
            | Ok (), _ -> acc
          in
          aux acc tl
    in
    aux (Ok ()) l
end
