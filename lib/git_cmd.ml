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

open Rresult
open Bos

let pp_header = Fmt.(styled `Blue string)

let header = ">>> "

let update repo branch roots excludes pins opam_switch remotes () =
  Exec.git_checkout ~repo branch
  >>= fun () ->
  Logs.info (fun l -> l "Running `duniverse opam`") ;
  Opam_cmd.init_duniverse repo branch roots excludes pins opam_switch remotes
    ()
  >>= fun () ->
  Logs.info (fun l -> l "Running `duniverse lock`") ;
  Dune_cmd.gen_dune_lock repo ()
  >>= fun () ->
  Logs.app (fun l -> l "%aGit committing the lockfiles." pp_header header) ;
  Exec.git_add_and_commit ~repo ~message:"update duniverse lockfiles"
    Cmd.(v (p Config.opam_lockfile) % p Config.duniverse_lockfile)

let pull repo branch () =
  let vendor_branch = Config.vendor_branch branch in
  Exec.is_git_repo_clean ~repo ()
  >>= function
  | false ->
      Logs.err (fun l ->
          l
            "Git repository %a has changes to the working copy.\n\
             Please commit them and rerun this command."
            Fmt.(styled `Cyan Fpath.pp)
            repo ) ;
      exit 1
  | true ->
      Logs.app (fun l ->
          l "%aGit checking out %a branch" pp_header header
            Fmt.(styled `Cyan string)
            vendor_branch ) ;
      Exec.git_checkout_or_branch ~repo vendor_branch
      >>= fun () ->
      let msg = Fmt.strf "merge from %s branch" branch in
      Exec.git_merge ~from:branch
        ~args:Cmd.(v "--commit" % "-m" % msg)
        ~repo ()
      >>= fun () ->
      Dune_cmd.gen_dune_upstream_branches repo ()
      (* TODO make this push a cli option with --push
  >>= fun () ->
  Exec.git_push ~args:(Cmd.v "-u") ~repo "origin" vendor_branch *)
      >>= fun () ->
      Logs.app (fun l ->
          l "%aGit checking out %a branch." pp_header header
            Fmt.(styled `Cyan string)
            branch ) ;
      Exec.git_checkout ~repo branch

let merge repo branch () =
  let vendor_branch = Config.vendor_branch branch in
  Logs.app (fun l ->
      l "%aGit checking out %a branch." pp_header header
        Fmt.(styled `Cyan string)
        branch ) ;
  Exec.git_checkout ~repo branch
  >>= fun () ->
  Logs.app (fun l ->
      l "%aGit merging from %a branch" pp_header header
        Fmt.(styled `Cyan string)
        vendor_branch ) ;
  Exec.git_merge ~from:vendor_branch ~args:Cmd.(v "--squash") ~repo ()
  >>= fun () ->
  let message =
    Fmt.strf "update vendor libraries from branch %s" vendor_branch
  in
  Exec.git_add_all_and_commit ~repo ~message ()
