let map_error ~f = function
  | Ok x -> Ok x
  | Error e -> Error (f e)

module List = struct
  let map ~f l =
    let rec aux acc = function
      | [] -> Ok (List.rev acc)
      | hd::tl ->
        ( match f hd with
          | Ok hd' -> aux (hd'::acc) tl
          | Error err -> Error err )
    in
    aux [] l
end
