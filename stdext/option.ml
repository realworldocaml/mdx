include Stdlib.Option

let value ~default = function None -> default | Some x -> x

let map ~f = function None -> None | Some x -> Some (f x)

let bind ~f = function None -> None | Some x -> f x

module O = struct
  let ( >>= ) opt f = bind ~f opt

  let ( >>| ) opt f = map ~f opt
end

let map_default ~f ~default = function None -> default | Some x -> f x
