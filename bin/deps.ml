(*
 * Copyright (c) 2020 Ulysse Gérard <ulysse@tarides.com>
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

let run (`Syntax syntax) files =
  List.iter (fun file ->
    let syntax = match syntax, Mdx.Syntax.infer ~file with
    | Some s, _
    | None, Some s -> s
    | None, None -> Fmt.failwith
      "Could not infer syntax from filename %s, use the --syntax \
      option to specify a syntax." file
    in
    let doc = Mdx.parse_file
      syntax
      file
    in
    let deps = Mdx.Dep.of_lines doc in
    let deps = List.map Mdx.Dep.to_string deps in
    let deps = String.concat " " deps in
    Printf.printf "%s: %s\n" file deps) files;
  0

open Cmdliner

let arg_files =
  let doc = "The files of which the dependencies will be listed." in
  let docv = "FILES" in
  Arg.(non_empty & pos_all file [] & info [] ~doc ~docv)

let cmd =
  let doc = "List the dependencies of the input files." in
  Term.(pure run $ Cli.syntax $ arg_files),
  Term.info "deps" ~doc
