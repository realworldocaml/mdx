open Astring

let pad_of_lines = function
  | []   -> 0
  | h::_ ->
    let i = ref 0 in
    while (!i < String.length h && h.[!i] = ' ') do incr i; done;
    !i

let pp_pad ppf = function
  | 0 -> ()
  | i -> Fmt.string ppf (String.v ~len:i (fun _ -> ' '))

let pp_lines pp = Fmt.(list ~sep:(unit "\n") pp)
let dump_string ppf s = Fmt.pf ppf "%S" s
