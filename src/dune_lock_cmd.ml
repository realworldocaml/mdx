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

let dune_repo_of_opam ?(verify_refs= true) opam =
  let dir = opam.Opam.name in
  match opam.Opam.dev_repo with
  | `Github (user, repo) ->
      let upstream = Fmt.strf "https://github.com/%s/%s.git" user repo in
      let ref = match opam.Opam.tag with None -> "master" | Some t -> t in
      if verify_refs then
        Cmd.git_ls_remote upstream
        >>= fun (tags, heads) ->
        if List.mem ref (heads @ tags) then Ok {Dune.dir; upstream; ref}
        else
          R.error_msg (Fmt.strf "%s#%s is not a branch or a tag" upstream ref)
      else Ok {Dune.dir; upstream; ref}
  | `Duniverse_fork repo ->
      let upstream = Fmt.strf "https://github.com/duniverse/%s.git" repo in
      let ref =
        match opam.Opam.tag with
        | None -> "duniverse-master"
        | Some t -> Fmt.strf "duniverse-%s" t
      in
      Ok {Dune.dir; upstream; ref}
  | x -> R.error_msg (Fmt.strf "TODO cannot handle %a" Opam.pp_opam opam)

let gen_dune_lock ifile ofile () =
  Bos.OS.File.read ifile
  >>= fun b ->
  Logs.debug (fun l -> l "Loading opam lockfile from %a" Fpath.pp ifile) ;
  let opam = Sexplib.Sexp.of_string b |> Opam.packages_of_sexp in
  let dune_packages = List.filter (fun o -> o.Opam.is_dune) opam.Opam.pkgs in
  Cmd.map dune_repo_of_opam dune_packages
  >>= fun repos ->
  let open Dune in
  let t = {repos} in
  Logs.debug (fun l -> l "%a" Dune.pp t) ;
  Ok ()
