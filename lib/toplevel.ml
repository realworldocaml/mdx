let src = Logs.Src.create "mdx"
module Log = (val Logs.src_log src : Logs.LOG)

open Astring
open Misc
open S

type t = toplevel

let dump_line ppf = function
  | #output as o -> Output.dump ppf o
  | `Command c   -> Fmt.pf ppf "`Command %a" Fmt.(Dump.list dump_string) c

let command (t : toplevel) = t.command
let output (t : toplevel) = t.output

let dump ppf ({ line; command; output } : t) =
  Fmt.pf ppf "@[{line=%d;@ command=%a;@ output=%a}@]"
    line
    Fmt.(Dump.list dump_string) command
    Fmt.(Dump.list Output.dump) output

let pp_command ?(pad=0) ppf (t : t) = match t.command with
  | [] -> ()
  | l  ->
    let sep ppf () = Fmt.pf ppf "\n%a  " pp_pad pad in
    Fmt.pf ppf "%a# %a\n" pp_pad pad Fmt.(list ~sep string) l

let pp ?pad ppf (t : t) =
  pp_command ?pad ppf t;
  pp_lines (Output.pp ?pad) ppf t.output

let of_lines ~line t =
  let pad = pad_of_lines t in
  let unpad line =
    if String.length line < pad then Fmt.failwith "invalide padding: %S" line
    else String.with_index_range line ~first:pad
  in
  let lines = List.map unpad t in
  let lines = String.concat ~sep:"\n" lines in
  let lines = Lexer.toplevel (Lexing.from_string lines) in
  Log.debug (fun l ->
      l "Toplevel.of_lines (pad=%d) %a" pad Fmt.(Dump.list dump_line) lines
    );
  let mk command line output = { command; line; output = List.rev output } in
  let rec aux command line output acc = function
    | []                  -> List.rev (mk command line output :: acc)
    | `Ellipsis as o :: t -> aux command line (o :: output) acc t
    | `Output _ as o :: t -> aux command line (o :: output) acc t
    | `Command cmd   :: t ->
      let line' = line + List.length cmd + List.length output in
      aux cmd line' [] (mk command line output :: acc) t
  in
  match lines with
  | `Command cmd :: t -> pad, aux cmd line [] [] t
  | _ -> Fmt.failwith "invalid toplevel block: %a" Fmt.(Dump.list string) t
