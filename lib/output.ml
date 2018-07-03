open Misc

type t = [`Output of string | `Ellipsis]

let dump ppf = function
  | `Output s -> Fmt.pf ppf "`Output %S" s
  | `Ellipsis -> Fmt.pf ppf "`Ellipsis"

let pp ?(pad=0) ppf = function
  | `Output s -> Fmt.pf ppf "%a%s\n" pp_pad pad s
  | `Ellipsis -> Fmt.pf ppf "%a...\n" pp_pad pad

let equal a b =
  let rec aux x y = match x, y with
    | [], []  | [`Ellipsis], _   | _, [`Ellipsis]  -> true
    | (`Ellipsis::a as x), (_::b as y) | (_::b as y), (`Ellipsis::a as x) ->
      aux x b || (* n+ matches: skip y's head *)
      aux a y    (* 0  match  : skip x's head *)
    | a::b, h::t -> a = h && aux b t
    | _ -> false
  in
  aux a b
