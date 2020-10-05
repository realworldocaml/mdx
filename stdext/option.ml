let value ~default = function
  | None -> default
  | Some x -> x

let map ~f = function
  | None -> None
  | Some x -> Some (f x)
