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

let prelude_file f =
  match String.cut ~sep:":" f with
  | None -> f
  | Some (_, f) -> f

let prepend_root r b = match r with
  | None   -> b
  | Some r -> Filename.concat r b

let print_rule ~nd ~prelude ~md_file ~ml_files ~dirs ~root options =
  let ml_files = String.Set.elements ml_files in
  let ml_files = List.map (prepend_root root) ml_files in
  let dirs = match root with
    | None      -> String.Set.elements dirs
    | Some root ->
      (* only keep absolute dirs *)
      let dirs = String.Set.filter (fun x -> not (Filename.is_relative x)) dirs in
      let dirs = String.Set.add root dirs in
      String.Set.elements dirs
  in
  let var_names =
    let f (cpt, acc) _ = cpt + 1, ("y" ^ string_of_int cpt) :: acc in
    List.fold_left f (0, []) ml_files |> snd
  in
  let pp_ml_deps fmt (var_name, ml_file) =
    Fmt.pf fmt "\         (:%s %s)" var_name ml_file
  in
  let pp_dir_deps fmt dir =
    Fmt.pf fmt "\         (source_tree %s)" dir
  in
  let pp_ml_diff fmt var =
    Fmt.pf fmt "\           (diff? %%{%s} %%{%s}.corrected)" var var
  in
  let prelude =
    let files = String.Set.of_list (List.map prelude_file prelude) in
    String.Set.elements files
    |> List.map (fun f -> Fmt.strf "         %s" f)
    |> String.concat ~sep:"\n"
  in
  let root = match root with None -> "" | Some r -> Fmt.strf "--root=%s " r in
  let deps =
    let sep1 = if var_names <> [] then "\n" else "" in
    let sep2 = if dirs <> [] && prelude <> "" then "\n" else "" in
    let x = Fmt.strf "%a%s%a%s%s"
        Fmt.(list ~sep:(unit "\n") pp_ml_deps) (List.combine var_names ml_files)
        sep1
        Fmt.(list ~sep:(unit "\n") pp_dir_deps) dirs
        sep2
        prelude
    in
    match x with
    | "" -> ""
    | s  -> "\n" ^ s
  in
  let pp name arg =
    Fmt.pr
      "\
(alias\n\
\ (name   %s)\n\
\ (deps   (:x %s)%s)\n\
\ (action (progn\n\
\           (run mdx test %a %s%s%%{x})\n\
\           (diff? %%{x} %%{x}.corrected)\n%a)))\n"
      name
      md_file
      deps
      Fmt.(list ~sep:(unit " ") string) options
      arg root
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

let add_opt e s = match e with None -> s | Some e -> String.Set.add e s

let run () md_file section direction prelude prelude_str root =
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
      let files, dirs, nd = acc in
      let nd = nd || match Mdx.Block.mode b with
        | `Non_det _ -> true
        | _          -> false
      in
      let source_trees = String.Set.of_list (Mdx.Block.source_trees b) in
      let dirs =
        dirs
        |> add_opt (Mdx.Block.directory b)
        |> String.Set.union source_trees
      in
      let files = add_opt (Mdx.Block.file b) files in
      files, dirs, nd
    | Block _ -> acc
  in
  let on_file file_contents items =
    let ml_files, dirs, nd =
      List.fold_left on_item (String.Set.empty, String.Set.empty, false) items
    in
    let options =
      List.map (Fmt.to_to_string pp_prelude) prelude @
      List.map (Fmt.to_to_string pp_prelude_str) prelude_str @
      [Fmt.to_to_string pp_direction direction]
    in
    print_rule ~md_file ~prelude ~nd ~ml_files ~dirs ~root options;
    file_contents
  in
  Mdx.run md_file ~f:on_file;
  0

open Cmdliner

let cmd =
  let doc = "Produce dune rules to synchronize markdown and OCaml files." in
  Term.(pure run
        $ Cli.setup $ Cli.file $ Cli.section $ Cli.direction
        $ Cli.prelude $ Cli.prelude_str $ Cli.root),
  Term.info "rule" ~doc
