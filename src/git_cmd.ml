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
open Astring

let update repo branch roots excludes pins opam_switch () =
  let branch = match branch with None -> "master" | Some b -> b in
  Cmd.git_checkout ~repo branch
  >>= fun () ->
  Logs.info (fun l -> l "Running `duniverse opam`") ;
  Opam_cmd.init_duniverse repo roots excludes pins opam_switch ()
  >>= fun () ->
  Logs.info (fun l -> l "Running `duniverse lock`") ;
  Dune_cmd.gen_dune_lock repo ()
  >>= fun () ->
  Cmd.git_add_and_commit ~repo ~message:"update duniverse lockfiles"
    Bos.Cmd.(v ".duniverse")

let pull repo branch () =
  let branch = match branch with None -> "master" | Some b -> b in
  let vendor_branch = Fmt.strf "duniverse-vendor-of-%s" branch in
  Cmd.git_checkout_or_branch ~repo vendor_branch
  >>= fun () ->
  let msg = Fmt.strf "merge from %s branch" branch in
  Cmd.git_merge ~from:branch ~args:Bos.Cmd.(v "--commit" % "-m" % msg) ~repo ()
  >>= fun () ->
  Dune_cmd.gen_dune_upstream_branches repo ()
  >>= fun () ->
  Cmd.git_push ~args:(Bos.Cmd.v "-u") ~repo "origin" vendor_branch
  >>= fun () -> Cmd.git_checkout ~repo branch

let merge repo branch () =
  let branch = match branch with None -> "master" | Some b -> b in
  let vendor_branch = Fmt.strf "duniverse-vendor-of-%s" branch in
  Cmd.git_checkout ~repo branch
  >>= fun () ->
  let message =
    Fmt.strf "update vendor libraries from branch %s" vendor_branch
  in
  Cmd.git_add_all_and_commit ~repo ~message ()
