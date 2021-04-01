module String = Stdlib.String
include StringLabels

module Map = Map.Make (struct
  type t = string

  let compare = String.compare

  let pp fmt t = Format.fprintf fmt "%S" t

  let show t = Format.asprintf "%a" pp t
end)

let starts_with ~prefix str =
  let str_len = String.length str in
  let prefix_len = String.length prefix in
  if str_len < prefix_len then false
  else
    let rec aux curr =
      if curr = prefix_len then true
      else prefix.[curr] = str.[curr] && aux (curr + 1)
    in
    aux 0
