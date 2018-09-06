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
open Bos

let dune_repo_of_opam ?(verify_refs = true) opam =
  let dir = Opam.(opam.package.name) in
  match opam.Opam.dev_repo with
  | `Github (user, repo) -> (
      let upstream = Fmt.strf "https://github.com/%s/%s.git" user repo in
      match opam.Opam.tag with
      | None ->
          Exec.git_default_branch ~remote:upstream ()
          >>= fun ref -> Ok {Dune.dir; upstream; ref}
      | Some ref -> Ok {Dune.dir; upstream; ref} )
  | `Duniverse_fork repo ->
      let upstream = Fmt.strf "git://github.com/dune-universe/%s.git" repo in
      let ref =
        match opam.Opam.tag with
        | None -> "duniverse-master"
        | Some t -> Config.duniverse_branch t
      in
      Logs.debug (fun l -> l "Duniverse fork: %s %s %s" dir upstream ref) ;
      Ok {Dune.dir; upstream; ref}
  | x -> R.error_msg (Fmt.strf "TODO cannot handle %a" Opam.pp_entry opam)

let dedup_git_remotes dunes =
  (* In the future we should be able to select the subtrees from different git
     remotes, but for now we error if we require different tags for the same
     git remote *)
  let open Dune in
  Logs.app (fun l -> l "Deduplicating the git remotes.") ;
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
            Logs.app (fun l ->
                l "Multiple entries found for %s with same tag: %s." upstream
                  (List.hd uniq_tags) )
          else
            let latest_tag =
              List.sort OpamVersionCompare.compare tags |> List.rev |> List.hd
            in
            Logs.app (fun l ->
                l
                  "Multiple entries found for %s with clashing tags: %s.\n\n\
                   We are selecting the latest version '%s' for use with all \
                   the packages that share the same development repo.\n\n\
                   In the future, we may implement some fancier subtree \
                   resolution to make it possible to support multiple tags \
                   from the same repository, but not yet."
                  upstream
                  (String.concat ~sep:", " uniq_tags)
                  latest_tag ) ;
            Hashtbl.replace by_repo upstream
              (List.map (fun d -> {d with ref= latest_tag}) dunes) )
    by_repo ;
  (* generate filtered dune list *)
  Ok
    (Hashtbl.fold
       (fun _ v acc ->
         ( List.sort
             (fun a b -> compare (String.length a.dir) (String.length b.dir))
             v
         |> List.hd )
         :: acc )
       by_repo [])

let gen_dune_lock repo () =
  Bos.OS.Dir.create Fpath.(repo // Config.duniverse_dir)
  >>= fun _ ->
  let ifile = Fpath.(repo // Config.opam_lockfile) in
  let ofile = Fpath.(repo // Config.duniverse_lockfile) in
  Opam.load ifile
  >>= fun opam ->
  let dune_packages = List.filter (fun o -> o.Opam.is_dune) opam.Opam.pkgs in
  Exec.map dune_repo_of_opam dune_packages
  >>= fun repos ->
  dedup_git_remotes repos
  >>= fun repos ->
  let open Dune in
  Dune.save ofile {repos}
  >>= fun () ->
  Logs.app (fun l -> l "Wrote Dune lockfile to %a." Fpath.pp ofile) ;
  Ok ()

let status repo target_branch () = Ok ()

let prune_recursive_vendors = Dune.(())

let gen_dune_upstream_branches repo () =
  Bos.OS.Dir.create Fpath.(repo // Config.duniverse_dir)
  >>= fun _ ->
  let ifile = Fpath.(repo // Config.duniverse_lockfile) in
  let open Dune in
  load ifile
  >>= fun dune ->
  let repos = dune.repos in
  Exec.iter
    (fun r ->
      let message = Fmt.strf "update vendor for %a" pp_repo r in
      let output_dir = Fpath.(Config.vendor_dir / r.dir) in
      Exec.git_archive ~output_dir ~remote:r.upstream ~tag:r.ref ()
      >>= fun () ->
      Exec.git_add_and_commit ~repo ~message Cmd.(v (p output_dir)) )
    repos
