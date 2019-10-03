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

let pp_dep fmt = function
  | `Named (name, path) -> Fmt.pf fmt "(:%s %s)" name path
  | `Source_tree path -> Fmt.pf fmt "(source_tree %s)" path
  | `Package p -> Fmt.pf fmt "(package %s)" p
  | `Path path -> Fmt.pf fmt "%s" path

let pp_action fmt = function
  | `Diff_corrected var -> Fmt.pf fmt "(diff? %%{%s} %%{%s}.corrected)" var var
  | `Run args -> Fmt.pf fmt "(run @[<hov 2>%a@])" Fmt.(list ~sep:sp string) args

let pp_rules ~nd ~prelude ~md_file ~ml_files ~dirs ~root ~requires
    fmt options =
  let ml_files = List.map (prepend_root root) (String.Set.elements ml_files) in
  let dirs = match root with
    | None      -> String.Set.elements dirs
    | Some root ->
      (* only keep absolute dirs *)
      let dirs = String.Set.filter (fun x -> not (Filename.is_relative x)) dirs in
      let dirs = String.Set.add root dirs in
      String.Set.elements dirs
  in
  let prelude_files =
    let files = String.Set.of_list (List.map prelude_file prelude) in
    String.Set.elements files
  in
  let var_names =
    let f (cpt, acc) _ = cpt + 1, ("y" ^ string_of_int cpt) :: acc in
    List.fold_left f (0, []) ml_files |> snd
  in
  let root = match root with None -> [] | Some r -> ["--root=" ^ r] in
  let deps =
    let packages =
      List.map (fun p -> `Package p) (String.Set.elements requires)
    and ml_files =
      List.map2 (fun name p -> `Named (name, p)) var_names ml_files
    and dirs = List.map (fun p -> `Source_tree p) dirs
    and prelude = List.map (fun p -> `Path p) prelude_files
    in
    `Named ("x", md_file) ::
    packages @
    ml_files @
    dirs @
    prelude
  in
  let actions arg =
    `Run ("ocaml-mdx" :: "test" :: options @ arg @ root @ ["%{x}"]) ::
    `Diff_corrected "x" ::
    List.map (fun v -> `Diff_corrected v) var_names
  in
  let pp fmt name arg =
    Fmt.pf fmt
      "\
(alias@\n\
\ (name   %s)@\n\
\ (deps   @[<v>%a@])@\n\
\ (action @[<hv 2>(progn@ %a)@]))@\n"
      name
      Fmt.(list ~sep:sp pp_dep) deps
      Fmt.(list ~sep:cut pp_action) (actions arg)
  in
  pp fmt "runtest" [];
  if nd then
    pp fmt "runtest-all" ["--non-deterministic"]

let print_format_dune_rules pp_rules =
  let open Unix in
  let read_pipe, write_pipe = pipe () in
  set_close_on_exec read_pipe;
  set_close_on_exec write_pipe;
  let dune_file_format =
    create_process "dune" [|"dune"; "format-dune-file"|] read_pipe stdout stderr
  in
  close read_pipe;
  let oc = out_channel_of_descr write_pipe in
  let fmt = Format.formatter_of_out_channel oc in
  pp_rules fmt ();
  close_out oc;
  match waitpid [] dune_file_format with
  | _, WEXITED 0 -> ()
  | _, WEXITED _ ->
    let msg =
      Format.asprintf
        "Failed to format the following rules with 'dune format-dune-file':\n%a"
        pp_rules ()
    in
    failwith msg
  | _, (WSIGNALED _ | WSTOPPED _) ->
    failwith "Child process 'dune format-dune-file' was interrupted"

let options_of_syntax = function
  | Some Mdx.Normal -> [ "--syntax=normal" ]
  | Some Mdx.Cram -> [ "--syntax=cram" ]
  | None -> []

let options_of_section = function
  | Some s -> [ Fmt.to_to_string (Fmt.fmt "--section %S") s ]
  | None -> []

let pp_direction fmt = function
  | `To_md -> Fmt.pf fmt "--direction=to-md"
  | `To_ml -> Fmt.pf fmt "--direction=to-ml"

let pp_prelude fmt s = Fmt.pf fmt "--prelude=%s" s
let pp_prelude_str fmt s = Fmt.pf fmt "--prelude-str %S" s

let add_opt e s = match e with None -> s | Some e -> String.Set.add e s

let run (`Setup ()) (`File md_file) (`Section section) (`Syntax syntax) (`Direction direction)
    (`Prelude prelude) (`Prelude_str prelude_str) (`Root root) =
  let active =
    let section = match section with
      | None   -> None
      | Some p -> Some (Re.Perl.compile_pat p)
    in
    fun b ->
      match section, Mdx.Block.section b with
      | None   , _      -> true
      | Some re, None   -> Re.execp re ""
      | Some re, Some s -> Re.execp re (snd s)
  in
  let on_item acc = function
    | Mdx.Section _ | Text _ -> acc
    | Block b when active b ->
      let files, dirs, nd, requires = acc in
      let requires =
        Mdx.Block.required_packages b
        |> List.fold_left (fun s e -> String.Set.add e s) requires
      in
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
      files, dirs, nd, requires
    | Block _ -> acc
  in
  let on_file file_contents items =
    let ml_files, dirs, nd, requires =
      let empty = String.Set.empty in
      List.fold_left on_item (empty, empty, false, empty) items
    in
    let options =
      List.map (Fmt.to_to_string pp_prelude) prelude @
      List.map (Fmt.to_to_string pp_prelude_str) prelude_str @
      [Fmt.to_to_string pp_direction direction] @
      options_of_syntax syntax @
      options_of_section section
    in
    let pp_rules fmt () =
      pp_rules ~md_file ~prelude ~nd ~ml_files ~dirs ~root ~requires fmt options
    in
    print_format_dune_rules pp_rules;
    file_contents
  in
  Mdx.run md_file ~f:on_file;
  0

open Cmdliner

let cmd =
  let doc = "Produce dune rules to synchronize markdown and OCaml files." in
  Term.(pure run
        $ Cli.setup $ Cli.file $ Cli.section $ Cli.syntax $ Cli.direction
        $ Cli.prelude $ Cli.prelude_str $ Cli.root),
  Term.info "rule" ~doc
