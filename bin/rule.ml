(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Astring

let src = Logs.Src.create "cram.rule"

module Log = (val Logs.src_log src : Logs.LOG)

let print_rule ~nd ~prelude ~md_file ~ml_files options =
  let pct = '%' in
  let ml_files = String.Set.elements ml_files in
  let var_names =
    let f (cpt, acc) _ = cpt + 1, ("y" ^ string_of_int cpt) :: acc in
    List.fold_left f (0, []) ml_files |> snd
  in
  let pp_ml_deps fmt (var_name, ml_file) =
    Fmt.pf fmt "\         (:%s %s)\n" var_name ml_file
  in
  let pp_ml_diff fmt var =
    Fmt.pf fmt "\           (diff? %c{%s} %c{%s}.corrected)" pct var pct var
  in
  let prelude = match prelude with
    | None   -> ""
    | Some f -> Fmt.strf "         %s\n" f
  in
  let pp name arg =
    Fmt.pr
      "\
(alias\n\
\ (name   %s)\n\
\ (deps   (:x %s)\n%a%s\
\         (package mdx))\n\
\ (action (progn\n\
\           (run mdx test %a %s%c{x})\n\
\           (diff? %c{x} %c{x}.corrected)\n%a)))\n"
      name
      md_file
      (Fmt.list ~sep:Fmt.nop pp_ml_deps) (List.combine var_names ml_files)
      prelude
      Fmt.(list ~sep:(unit " ") string) options
      arg
      pct pct pct
      (Fmt.list ~sep:Fmt.cut pp_ml_diff) var_names
  in
  pp "runtest" "";
  if nd then pp "runtest-all" "--non-deterministic "

let pp_direction fmt = function
  | `Infer_timestamp -> Fmt.pf fmt "--direction=infer-timestamp"
  | `To_md -> Fmt.pf fmt "--direction=to-md"
  | `To_ml -> Fmt.pf fmt "--direction=to-ml"

let pp_prelude fmt s = Fmt.pf fmt "--prelude=%s" s
let pp_prelude_str fmt s = Fmt.pf fmt "--prelude-str=%S" s

let use_dune t =
  List.exists (fun t ->
      match t.Mdx.Cram.command with
      | []   -> false
      | h::_ -> match Astring.String.cuts ~sep:" " ~empty:false h with
        | "dune"::_ -> true
        | _ -> false
    ) t

let run () md_file section direction prelude prelude_str =
  let section = match section with
    | None   -> None
    | Some p -> Some (Re.Perl.compile_pat p)
  in
  let active b = match section, Mdx.Block.section b with
    | None   , _      -> true
    | Some re, None   -> Re.execp re ""
    | Some re, Some s -> Re.execp re (snd s)
  in
  let on_item acc = function
    | Mdx.Section _ | Text _ -> acc
    | Block b when active b ->
      (match Mdx.Block.value b, Mdx.Block.file b, Mdx.Block.mode b with
       | (OCaml | Toplevel _), Some ml_file, mode ->
         Log.debug (fun l -> l "rule: (md: %s) (ml: %s)" md_file ml_file);
         let files, nd = acc in
         let nd = nd || match mode with `Non_det _ -> true | _ -> false in
         let files = String.Set.add ml_file files in
         files, nd
       | Cram t, _, mode ->
         let files, nd = acc in
         let files =
           if not (use_dune t.tests) then files
           else
             let dir = match Mdx.Block.directory b with
               | None -> "."
               | Some d -> d
             in
             let dune_files =
               List.map (Filename.concat dir)
                 [ "dune"; "dune-project"; "dune-workspace" ]
             in
             let files = List.fold_right String.Set.add dune_files files in
             files
         in
         let nd = nd || match mode with `Non_det _ -> true | _ -> false in
         files, nd
       | _ -> acc)
    | Block _ -> acc
  in
  let on_file file_contents items =
    let ml_files, nd = List.fold_left on_item (String.Set.empty, false) items in
    let option pp = function None -> [] | Some s -> [Fmt.to_to_string pp s] in
    let options =
      (option pp_prelude prelude) @
      (option pp_prelude_str prelude_str) @
      [Fmt.to_to_string pp_direction direction]
    in
    print_rule ~md_file ~prelude ~nd ~ml_files options;
    file_contents
  in
  Mdx.run md_file ~f:on_file;
  0

open Cmdliner

let cmd =
  let doc = "Produce dune rules to synchronize markdown and OCaml files." in
  Term.(pure run
        $ Cli.setup $ Cli.file $ Cli.section $ Cli.direction
        $ Cli.prelude $ Cli.prelude_str),
  Term.info "rule" ~doc
