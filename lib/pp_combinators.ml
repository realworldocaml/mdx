module Ocaml = struct
  let string fmt s = Format.fprintf fmt "%S" s

  let option ?(brackets = true) pp_a fmt = function
    | None -> Format.fprintf fmt "None"
    | Some a when brackets -> Format.fprintf fmt "Some (%a)" pp_a a
    | Some a -> Format.fprintf fmt "Some %a" pp_a a

  let list pp_a fmt = function
    | [] -> Format.fprintf fmt "[]"
    | [ a ] -> Format.fprintf fmt "[%a]" pp_a a
    | l ->
        let pp_sep fmt () = Format.fprintf fmt ";@ " in
        Format.fprintf fmt "@[<hov 2>[@ %a]@]" (Format.pp_print_list ~pp_sep pp_a) l
end
