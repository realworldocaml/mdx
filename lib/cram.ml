open Astring
open Misc
open S

type t = cram

let dump ppf (t : t) =
  Fmt.pf ppf
    "{@[command: %a;@ output: %a;@ exit_code: %d@]}"
    Fmt.(Dump.list dump_string) t.command
    Fmt.(Dump.list Output.dump) t.output
    t.exit_code

let pp_command ?(pad=0) ppf (t : t) = match t.command with
  | [] -> ()
  | l  ->
    let sep ppf () = Fmt.pf ppf "\\\n%a> " pp_pad pad in
    Fmt.pf ppf "%a$ %a\n" pp_pad pad Fmt.(list ~sep string) l

let pp_exit_code ?(pad=0) ppf n =
  if n <> 0 then Fmt.pf ppf "%a✘ exit %d\n" pp_pad pad n

let pp ?pad ppf (t : t) =
  pp_command ?pad ppf t;
  pp_lines (Output.pp ?pad) ppf t.output;
  pp_exit_code ?pad ppf t.exit_code

let pad_of_lines = function
  | []   -> 0
  | h::_ ->
    let i = ref 0 in
    while (!i < String.length h && h.[!i] = ' ') do incr i; done;
    !i

let of_lines t =
  let pad = pad_of_lines t in
  let unpad line =
    if String.length line < pad then Fmt.failwith "invalide padding: %S" line
    else String.with_index_range line ~first:pad
  in
  let lines = List.rev_map unpad t in
  let lines =
    List.rev_map (fun s -> Lexer.cram (Lexing.from_string s)) lines
  in
  let mk command output exit_code =
    { command; output = List.rev output; exit_code }
  in
  let rec aux command output acc = function
    | []                  -> List.rev (mk command output 0 :: acc)
    | `Exit i        :: t -> aux [] [] (mk command output i :: acc) t
    | `Ellipsis as o :: t -> aux command (o :: output) acc t
    | `Command cmd   :: t -> aux cmd [] (mk command output 0 :: acc) t
    | `Output _ as o :: t -> aux command (o :: output) acc t
  in
  match lines with
  | `Command cmd :: t -> pad, aux cmd [] [] t
  | _ -> Fmt.failwith "invalid cram block: %a" Fmt.(Dump.list string) t

let exit_code t = t.exit_code

(* http://tldp.org/LDP/abs/html/here-docs.html *)
let use_heredoc (t : t) =
  String.cut (List.hd t.command) ~sep:"<<" <> None

let command_line t =
  if not (use_heredoc t) then
    String.concat ~sep:" " t.command
  else
    String.concat ~sep:"\n" t.command
