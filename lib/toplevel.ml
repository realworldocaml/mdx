open Astring
open Misc
open S

type t = toplevel

let command (t : toplevel) = t.command
let output (t : toplevel) = t.output

let dump ppf (t : t) =
  Fmt.pf ppf "@[{command=%a;@ output=%a}@]"
    Fmt.(Dump.list dump_string) t.command
    Fmt.(Dump.list Output.dump) t.output

let pp_command ?(pad=0) ppf (t : t) = match t.command with
  | [] -> ()
  | l  ->
    let sep ppf () = Fmt.pf ppf "\\\n%a> " pp_pad pad in
    Fmt.pf ppf "%a# %a\n" pp_pad pad Fmt.(list ~sep string) l

let pp ?pad ppf (t : t) =
  pp_command ?pad ppf t;
  pp_lines (Output.pp ?pad) ppf t.output

let of_lines t =
  let pad = pad_of_lines t in
  let unpad line =
    if String.length line < pad then Fmt.failwith "invalide padding: %S" line
    else String.with_index_range line ~first:pad
  in
  let lines = List.rev_map unpad t in
  let lines =
    List.rev_map (fun s -> Lexer.toplevel (Lexing.from_string s)) lines
  in
  let mk command output = { command; output = List.rev output } in
  let rec aux command output acc = function
    | []                  -> List.rev (mk command output :: acc)
    | `Ellipsis as o :: t -> aux command (o :: output) acc t
    | `Command cmd   :: t -> aux cmd [] (mk command output :: acc) t
    | `Output _ as o :: t -> aux command (o :: output) acc t
  in
  match lines with
  | `Command cmd :: t -> pad, aux cmd [] [] t
  | _ -> Fmt.failwith "invalid cram block: %a" Fmt.(Dump.list string) t
