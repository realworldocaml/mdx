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

let src = Logs.Src.create "cram.rule"

module Log = (val Logs.src_log src : Logs.LOG)

let print_rule ~md_file ~ml_files direction =
  let pct = '%' in
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
  let pp_direction fmt = function
    | `Infer_timestamp -> Fmt.pf fmt "infer-timestamp"
    | `To_md -> Fmt.pf fmt "to-md"
    | `To_ml -> Fmt.pf fmt "to-ml"
  in
  Fmt.pr
    "\
(alias\n\
\ (name   runtest)\n\
\ (deps   (:x %s)\n%a\
\         (package mdx))\n\
\ (action (progn\n\
\           (run mdx test --direction=%a %c{x})\n\
\           (diff? %c{x} %c{x}.corrected)\n%a)))\n"
    md_file
    (Fmt.list ~sep:Fmt.nop pp_ml_deps) (List.combine var_names ml_files)
    pp_direction direction
    pct pct pct
    (Fmt.list ~sep:Fmt.cut pp_ml_diff) var_names

let run () md_file section direction =
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
      (match Mdx.Block.value b, Mdx.Block.file b with
       | (OCaml | Toplevel _), Some ml_file ->
         Log.debug (fun l -> l "rule: (md: %s) (ml: %s)" md_file ml_file);
         if List.mem ml_file acc then acc
         else ml_file :: acc
       | _ -> acc)
    | Block _ -> acc
  in
  let on_file file_contents items =
    let ml_files = List.fold_left on_item [] items in
    print_rule ~md_file ~ml_files direction;
    file_contents
  in
  Mdx.run md_file ~f:on_file;
  0

open Cmdliner

let cmd =
  let doc = "Produce dune rules to synchronize markdown and OCaml files." in
  Term.(pure run $ Cli.setup $ Cli.file $ Cli.section $ Cli.direction),
  Term.info "rule" ~doc
