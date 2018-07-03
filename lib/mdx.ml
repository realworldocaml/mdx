let src = Logs.Src.create "mdx"
module Log = (val Logs.src_log src : Logs.LOG)

module Output = Output
module Cram = Cram
module Toplevel = Toplevel
module Block = Block

include S

type t = line list

let dump_line ppf (l: line) = match l with
  | Block b        -> Fmt.pf ppf "Block %a" Block.dump b
  | Section (d, s) -> Fmt.pf ppf "Section (%d, %S)" d s
  | Text s         -> Fmt.pf ppf "Text %S" s

let dump = Fmt.Dump.list dump_line

let pp_line ppf (l: line) = match l with
  | Block b        -> Fmt.pf ppf "%a\n" Block.pp b
  | Section (d, s) -> Fmt.pf ppf "%s %s\n" (String.make d '#') s
  | Text s         -> Fmt.pf ppf "%s\n" s

let pp ppf t = Fmt.pf ppf "%a\n" Fmt.(list ~sep:(unit "\n") pp_line) t

let to_string = Fmt.to_to_string pp

let section_of_line = function
  | Section s -> Some s
  | Text _    -> None
  | Block b   -> b.section

let filter_section re (t: t) =
  match
    List.filter (fun l -> match section_of_line l with
        | None        -> false
        | Some (_, s) -> Re.execp re s
      )  t
  with
  | [] -> None
  | l  -> Some l

let parse_lexbuf l = Lexer.token l
let parse_file f = Lexer.token (snd (Common.init f))
let of_string s = parse_lexbuf (Lexing.from_string s)

let eval = function
  | Section _ | Text _ as x -> x
  | Block t as x ->
    match t.header with
    | Some ("sh" | "bash") ->
      let value = Block.cram t.contents in
      Block { t with value }
    | Some "ocaml" ->
      if Block.is_raw_ocaml t then x
      else
        let value = Block.toplevel t.contents in
        Block { t with value }
    | _ -> x

let run ~f n =
  Common.run_expect_test n ~f:(fun c l ->
      let items = parse_lexbuf l in
      let items = List.map eval items in
      Log.debug (fun l -> l "run @[%a@]" dump items);
      f c items
    )
