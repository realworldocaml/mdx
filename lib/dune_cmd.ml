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

let pp_header = Fmt.(styled `Blue string)

let header = "==> "

let dune_repo_of_opam opam =
  let dir = Opam.(opam.package.name) in
  match opam.Opam.dev_repo with
  | `Git upstream -> (
    match opam.Opam.tag with
    | None ->
        Exec.git_default_branch ~remote:upstream () >>= fun ref -> Ok { Dune.dir; upstream; ref }
    | Some ref -> Ok { Dune.dir; upstream; ref } )
  | _ -> R.error_msg (Fmt.strf "TODO cannot handle %a" Opam.pp_entry opam)

let dedup_git_remotes dunes =
  (* In the future we should be able to select the subtrees from different git
     remotes, but for now we error if we require different tags for the same
     git remote *)
  let open Dune in
  Logs.app (fun l ->
      l "%aDeduplicating the %a git remotes." pp_header header
        Fmt.(styled `Green int)
        (List.length dunes) );
  let by_repo = Hashtbl.create 7 in
  List.iter
    (fun dune ->
      match Hashtbl.find by_repo dune.upstream with
      | rs -> Hashtbl.replace by_repo dune.upstream (dune :: rs)
      | exception Not_found -> Hashtbl.add by_repo dune.upstream [ dune ] )
    dunes;
  Hashtbl.iter
    (fun upstream dunes ->
      match dunes with
      | [] -> assert false
      | [ _ ] -> ()
      | dunes ->
          let tags = List.map (fun { ref; _ } -> ref) dunes in
          let uniq_tags = List.sort_uniq String.compare tags in
          if List.length uniq_tags = 1 then
            Logs.info (fun l ->
                l
                  "%aMultiple entries found for package %a with same tag, so just adding a single \
                   entry: %s."
                  pp_header header
                  Fmt.(styled `Yellow string)
                  upstream (List.hd uniq_tags) )
          else
            let latest_tag = List.sort OpamVersionCompare.compare tags |> List.rev |> List.hd in
            Logs.app (fun l ->
                l
                  "%a@[Multiple entries found for %a with clashing tags @[<1>'%a'@], so selected \
                   @[<1>'%a'@]%a@]"
                  pp_header header
                  Fmt.(styled `Yellow string)
                  upstream
                  Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
                  uniq_tags
                  Fmt.(styled `Green string)
                  latest_tag Fmt.text
                  " for use with the opam packages that share the same repo. We may implement \
                   some fancier subtree resolution to make it possible to support multiple tags \
                   from the same repository, but not yet." );
            Hashtbl.replace by_repo upstream
              (List.map (fun d -> { d with ref = latest_tag }) dunes) )
    by_repo;
  (* generate filtered dune list *)
  Ok
    (Hashtbl.fold
       (fun _ v acc ->
         (List.sort (fun a b -> compare (String.length a.dir) (String.length b.dir)) v |> List.hd)
         :: acc )
       by_repo [])

let log_invalid_packages packages =
  let open Opam in
  List.iter
    (function
      | { dev_repo = `Error msg; package; _ } ->
          Logs.warn (fun l -> l "Do not know how to deal with %a: %s" pp_package package msg)
      | _ -> () )
    packages

let package_is_valid { Opam.dev_repo; _ } = match dev_repo with `Error _ -> false | _ -> true

let filter_invalid_packages pkgs =
  Logs.app (fun l ->
      l "%aEliminating dependencies that are not understood by the Duniverse." pp_header header );
  log_invalid_packages pkgs;
  List.filter package_is_valid pkgs

let gen_dune_upstream_branches repo =
  let ifile = Fpath.(repo // Config.duniverse_file) in
  let open Duniverse in
  load ~file:ifile >>= fun dune ->
  let repos = dune.repos in
  Exec.iter
    (fun r ->
      let open Dune in
      let output_dir = Fpath.(Config.vendor_dir / r.dir) in
      Logs.app (fun l ->
          l "%aPulling sources for %a." pp_header header Fmt.(styled `Cyan Fpath.pp) output_dir );
      let output_dir = Fpath.(Config.vendor_dir / r.dir) in
      Exec.git_archive ~output_dir ~remote:r.upstream ~tag:r.ref () )
    repos
