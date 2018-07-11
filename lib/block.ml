open S

type value = block_value
type t = block

let dump_string ppf s = Fmt.pf ppf "%S" s
let dump_strings = Fmt.Dump.list dump_string
let dump_section = Fmt.(Dump.pair int string)

let dump_value ppf = function
  | Raw -> Fmt.string ppf "Raw"
  | Cram { pad; tests } ->
    Fmt.pf ppf "@[Cram@ {pad=%d;@ tests=%a}@]"
      pad Fmt.(Dump.list Cram.dump) tests
  | Toplevel { pad; tests } ->
    Fmt.pf ppf "@[Toplevel { pad=%d;@ tests=%a}@]"
      pad Fmt.(Dump.list Toplevel.dump) tests

let dump ppf { file; line; section; labels; header; contents; value } =
  Fmt.pf ppf
    "{@[file: %s;@ line: %d;@ section: %a;@ labels: %a;@ header: %a;@
        contents: %a;@ value: %a@]}"
    file line
    Fmt.(Dump.option dump_section) section
    dump_strings labels
    Fmt.(Dump.option string) header
    Fmt.(Dump.list dump_string) contents
    dump_value value

let pp_lines pp = Fmt.(list ~sep:(unit "\n") pp)
let pp_contents ppf t = Fmt.pf ppf "%a\n" (pp_lines Fmt.string) t.contents
let pp_footer ppf () = Fmt.string ppf "```\n"

let pp_header ppf t =
  let pp_labels ppf () = match t.labels with
    | [] -> ()
    | l  -> Fmt.pf ppf " %a" Fmt.(list ~sep:(unit ",") string) l
  in
  Fmt.pf ppf "```%a%a\n" Fmt.(option string) t.header pp_labels ()

let pp ppf b =
  pp_header ppf b;
  pp_contents ppf b;
  pp_footer ppf ()

let mode t = match t.labels with
  | [] -> `Normal
  | ["non-deterministic"]
  | ["non-deterministic=output"]  -> `Non_det `Output
  | ["non-deterministic=command"] -> `Non_det `Command
  | _ ->
    Fmt.failwith "invalid labels: '%a'" Fmt.(list ~sep:(unit ", ") string) t.labels

let value t = t.value
let section t = t.section
let header t = t.header

let cram lines =
  let pad, tests = Cram.of_lines lines in
  Cram { pad; tests }

let is_raw_ocaml b =
  match b.header, b.contents with
  | Some "ocaml", h::_ ->
    let h = String.trim h in
    String.length h > 1 && h.[0] <> '#'
  | _ -> false

let toplevel ~file ~line lines =
  let pad, tests = Toplevel.of_lines ~line ~file lines in
  Toplevel { pad; tests }

let eval t =
  match t.header with
  | Some ("sh" | "bash") ->
    let value = cram t.contents in
    { t with value }
  | Some "ocaml" ->
    if is_raw_ocaml t then t
    else
      let value = toplevel ~file:t.file ~line:t.line t.contents in
      { t with value }
  | _ -> t


let ends_by_semi_semi c = match List.rev c with
  | h::_ ->
    let len = String.length h in
    len > 2 && h.[len-1] = ';' && h.[len-2] = ';'
  | _ -> false

let executable_contents b =
  let contents =
    if is_raw_ocaml b then b.contents
    else match b.value with
      | Raw | Cram _ -> []
      | Toplevel { tests; pad } ->
        List.flatten (
          List.map (fun (t:toplevel) ->
              match t.command with
              | [] -> []
              | cs ->
                let mk s = String.make (pad+2) ' ' ^ s in
                Fmt.strf "#%d %S" t.line b.file ::
                List.map mk cs
            ) tests)
  in
  if contents = [] || ends_by_semi_semi contents then contents
  else contents @ [";;"]
