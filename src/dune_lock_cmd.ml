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

let dedup_git_remotes dunes =
  (* In the future we should be able to select the subtrees from different git
     remotes, but for now we error if we require different tags for the same
     git remote *)
  let open Dune in
  Logs.info (fun l -> l "Deduplicating the git remotes") ;
  let by_repo = Hashtbl.create 7 in
  List.iter
    (fun dune ->
      match Hashtbl.find by_repo dune.upstream with
      | rs -> Hashtbl.replace by_repo dune.upstream (dune :: rs)
      | exception Not_found -> Hashtbl.add by_repo dune.upstream [dune] )
    dunes ;
  Hashtbl.iter
    (fun upstream dunes ->
      match dunes with
      | [] -> assert false
      | [_] -> ()
      | dunes ->
          let tags = List.map (fun {ref} -> ref) dunes in
          let uniq_tags = List.sort_uniq String.compare tags in
          if List.length uniq_tags = 1 then
            Logs.info (fun l ->
                l "Multiple entries found for %s with same tag: %s." upstream
                  (List.hd uniq_tags) )
          else
            Logs.err (fun l ->
                l "Multiple entries found for %s with clashing tags: %s"
                  upstream
                  (String.concat ~sep:", " uniq_tags) ) )
    by_repo ;
  Ok ()

let gen_dune_lock ifile ofile () =
  Bos.OS.File.read ifile
  >>= fun b ->
  Logs.debug (fun l -> l "Loading opam lockfile from %a" Fpath.pp ifile) ;
  let opam = Sexplib.Sexp.of_string b |> Opam.packages_of_sexp in
  let dune_packages = List.filter (fun o -> o.Opam.is_dune) opam.Opam.pkgs in
  Cmd.map dune_repo_of_opam dune_packages
  >>= fun repos ->
  dedup_git_remotes repos
  >>= fun () ->
  let open Dune in
  let t = {repos} in
  Bos.OS.File.write ofile (Fmt.strf "%a\n" Dune.pp t)
  >>= fun () ->
  Logs.info (fun l -> l "Wrote Dune lockfile to %a" Fpath.pp ofile) ;
  Ok ()
