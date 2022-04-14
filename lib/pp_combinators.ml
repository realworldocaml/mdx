module Ocaml = struct
  let string fmt s = Format.fprintf fmt "%S" s
  let bool fmt b = Format.fprintf fmt "%B" b

  let option ?(brackets = true) pp_a fmt = function
    | None -> Format.fprintf fmt "None"
    | Some a when brackets -> Format.fprintf fmt "Some (%a)" pp_a a
    | Some a -> Format.fprintf fmt "Some %a" pp_a a

  let list pp_a fmt = function
    | [] -> Format.fprintf fmt "[]"
    | [ a ] -> Format.fprintf fmt "[%a]" pp_a a
    | l ->
        let pp_sep fmt () = Format.fprintf fmt ";@ " in
        Format.fprintf fmt "@[<hov 2>[@ %a@ ]@]"
          (Format.pp_print_list ~pp_sep pp_a)
          l

  let pair pp_a pp_b fmt (a, b) =
    Format.fprintf fmt "@[<hov 2>(@ %a,@ %a@ )@]" pp_a a pp_b b
end

module Opam = struct
  module type Printable = sig
    type t

    val pp : t Fmt.t
  end

  module Make_Set (M : OpamStd.SET) (P : Printable with type t = M.elt) : sig
    val pp : ?sep:unit Fmt.t -> M.t Fmt.t
  end = struct
    let pp ?sep = Fmt.iter ?sep M.iter P.pp
  end
end
