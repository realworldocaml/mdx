(*
 * Copyright (c) 2018 Ulysse Gérard <ulysse@gtarides.com>
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

let run files =
  List.iter (fun f ->
    let doc = Mdx.parse_file
      Mdx.Normal (* todo infer syntax, works only with md for now *)
      f
    in
    let deps = Mdx.Dep.of_lines doc in
    let deps = List.map Mdx.Dep.to_string deps in
    let deps = String.concat " " deps in
    Printf.printf "%s: %s\n" f deps) files;
  0


open Cmdliner

let arg_files =
  let doc = "The files of which the dependencies will be listed." in
  let docv = "FILES" in
  Arg.(non_empty & pos_all file [] & info [] ~doc ~docv)

let cmd =
  let doc = "List the dependencies of the input files." in
  Term.(pure run $ arg_files),
  Term.info "deps" ~doc
