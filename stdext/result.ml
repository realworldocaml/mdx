include Stdlib.Result

let bind ~f = function Error _ as err -> err | Ok x -> f x

let map ~f = function Error _ as err -> err | Ok x -> Ok (f x)

module O = struct
  let ( >>= ) res f = bind ~f res

  let ( >>| ) res f = map ~f res

  let ( let* ) = ( >>= )

  let ( let+ ) = ( >>| )
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
    match l with
    | [] -> Ok ()
    | hd :: tl ->
        let* () = f hd in
        iter ~f tl

  let all =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | t :: l ->
          let* x = t in
          loop (x :: acc) l
    in
    fun l -> loop [] l

  let rec fold_left t ~f ~init =
    match t with
    | [] -> Ok init
    | x :: xs ->
        let* init = f init x in
        fold_left xs ~f ~init
end
