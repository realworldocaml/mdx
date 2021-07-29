include Stdlib.Result

let bind ~f = function Error _ as err -> err | Ok x -> f x

let map ~f = function Error _ as err -> err | Ok x -> Ok (f x)

module O = struct
  let ( >>= ) res f = bind ~f res

  let ( >>| ) res f = map ~f res
end

let map_error ~f = function Ok x -> Ok x | Error e -> Error (f e)

module List = struct
  open O
  module List = Stdlib.List

  let map ~f l =
    let rec aux acc = function
      | [] -> Ok (List.rev acc)
      | hd :: tl -> (
          match f hd with
          | Ok hd' -> aux (hd' :: acc) tl
          | Error err -> Error err)
    in
    aux [] l

  let rec iter ~f l =
    match l with [] -> Ok () | hd :: tl -> f hd >>= fun () -> iter ~f tl

  let all =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | t :: l -> t >>= fun x -> loop (x :: acc) l
    in
    fun l -> loop [] l

  let rec fold_left t ~f ~init =
    match t with
    | [] -> Ok init
    | x :: xs -> f init x >>= fun init -> fold_left xs ~f ~init
end
