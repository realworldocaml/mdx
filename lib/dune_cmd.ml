(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

open Types
open Rresult

let pp_header = Fmt.(styled `Blue string)

let header = "==> "

let log_invalid_packages packages =
  let open Opam in
  List.iter
    (function
      | { dev_repo = `Error msg; package; _ } ->
          Logs.warn (fun l -> l "Do not know how to deal with %a: %s" pp_package package msg)
      | _ -> () )
    packages

let gen_dune_upstream_branches repo =
  let open Duniverse in
  let open Duniverse.Element in
  let ifile = Fpath.(repo // Config.duniverse_file) in
  load ~file:ifile >>= fun dune ->
  Exec.iter
    (function
      | Opam _ -> Ok ()
      | Repo { dir; upstream; ref } ->
          let output_dir = Fpath.(Config.vendor_dir / dir) in
          Logs.app (fun l ->
              l "%aPulling sources for %a." pp_header header Fmt.(styled `Cyan Fpath.pp) output_dir
          );
          let output_dir = Fpath.(Config.vendor_dir / dir) in
          Exec.git_archive ~output_dir ~remote:upstream ~tag:ref () )
    dune.content
