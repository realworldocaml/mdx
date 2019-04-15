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

open Bos
open Rresult

let load_sexp label conv file =
  Logs.debug (fun l -> l "Reading file %a for %s" Fpath.pp file label) ;
  OS.File.read file
  >>= fun b ->
  try Sexplib.Sexp.of_string b |> conv |> R.ok with exn ->
    R.error_msg
      (Fmt.strf "Error parsing %a: %s" Fpath.pp file (Printexc.to_string exn))

let save_sexp label conv file v =
  Logs.debug (fun l -> l "Writing file %a for %s" Fpath.pp file label) ;
  let b = Sexplib.Sexp.to_string_hum (conv v) in
  OS.File.write file b
