(*
 * Copyright (c) 2021 Guillaume Petiot <guillaume@tarides.com>
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

open Mdx
open Util.Result.Infix

let call_ocamlformat ~filename:f ~syntax lines =
  let aux () =
    let cmd = Bos.Cmd.v "ocamlformat" in
    Bos.OS.Cmd.get_tool cmd >>= fun _path ->
    Bos.OS.Dir.current () >>= fun dir ->
    let filename = Fpath.(dir / f |> to_string) in
    let input = Bos.OS.Cmd.in_string (Astring.String.concat ~sep:"\n" lines) in
    let cmd = Bos.Cmd.(v "ocamlformat" % syntax % "--name" % filename % "-") in
    let out = Bos.OS.Cmd.run_io cmd input in
    Bos.OS.Cmd.to_lines out
  in
  match aux () with Ok result -> result | Error _ -> lines

let format_contents ~filename (b : Block.t) =
  match b.value with
  | Raw _ | Cram _ | Include _ -> b.contents
  | OCaml _ -> call_ocamlformat ~filename ~syntax:"--impl" b.contents
  | Toplevel _ -> call_ocamlformat ~filename ~syntax:"--repl-file" b.contents

let pp_line fs ~filename ~contents:_ = function
  | Section (i, title) -> Fmt.pf fs "%s %s" (String.make i '#') title
  | Text s -> Fmt.pf fs "%s" s
  | Block b -> Block.pp fs { b with contents = format_contents ~filename b }

let pp_file fs ~filename ~contents lines =
  Fmt.list
    ~sep:(fun fs () -> Fmt.pf fs "\n")
    (pp_line ~filename ~contents)
    fs lines

let format_file ~filename contents lines =
  Fmt.str "%a" (pp_file ~filename ~contents) lines

type file = { first : Mdx.Part.file; current : Mdx.Part.file }

let files : (string, file) Hashtbl.t = Hashtbl.create 8

let has_changed ~force_output { first; current } =
  let contents = Mdx.Part.contents current in
  if contents = Mdx.Part.contents first && force_output = false then None
  else Some contents

let write_parts ~force_output file parts =
  let output_file = file ^ ".corrected" in
  match has_changed ~force_output parts with
  | None -> if Sys.file_exists output_file then Sys.remove output_file
  | Some c ->
      let oc = open_out output_file in
      output_string oc c;
      flush oc;
      close_out oc

let run (`Setup ()) (`Syntax syntax) (`File file) (`Force_output force_output)
    (`Output output) =
  let f = format_file ~filename:file in
  (match (output : Cli.output option) with
  | Some Stdout -> run_to_stdout ?syntax ~f file
  | Some (File outfile) -> run_to_file ?syntax ~outfile ~f file
  | None -> run ?syntax ~force_output file ~f)
  >>! fun () ->
  Hashtbl.iter (write_parts ~force_output) files;
  0

let cmd =
  let open Cmdliner in
  let doc = "Format markdown files with ocamlformat." in
  let exits = Term.default_exits in
  ( Term.(
      pure run $ Cli.setup $ Cli.syntax $ Cli.file $ Cli.force_output
      $ Cli.output),
    Term.info "format" ~doc ~exits )
