include Stdlib.Option

module Op = struct
  let ( let* ) o f = match o with None -> None | Some x -> f x

  let ( let+ ) o f = match o with None -> None | Some x -> Some (f x)
end
