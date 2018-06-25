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

open Types.Opam
open Rresult
open Astring
open Config

let split_opam_name_and_version b =
  match String.cut ~sep:"." b with
  | None -> R.error_msg (Fmt.strf "Unable to find version string in %s" b)
  | Some r -> Ok r

let strip_ext fname =
  let open Fpath in
  let fname = v fname |> rem_ext in
  let fname = if has_ext "tar" fname then rem_ext fname else fname in
  to_string fname

let tag_from_archive archive =
  let uri = Uri.of_string archive in
  let path = String.cuts ~empty:false ~sep:"/" (Uri.path uri) in
  let parse_err () =
    Logs.err (fun l -> l "Unable to classify archive %s" archive) ;
    None
  in
  let tag_of_file ?(prefix= "") f =
    match strip_ext f |> String.cut ~rev:true ~sep:"-" with
    | None -> parse_err ()
    | Some (n, v) -> Some (prefix ^ v)
  in
  let tag_of_last_path ?prefix () =
    List.rev path |> List.hd |> tag_of_file ?prefix
  in
  match Uri.host uri with
  | Some "github.com" -> (
    match path with
    | [u; r; "releases"; "download"; v; archive] -> Some v
    | [u; r; "archive"; archive] -> Some (strip_ext archive)
    | _ -> if Uri.scheme uri = Some "git+https" then None else parse_err () )
  | Some "ocaml.janestreet.com" -> (
    match path with
    | ["ocaml-core"; ver; "files"; f] -> tag_of_file f
    | ["janestreet"; r; "releases"; "download"; v; f] -> Some v
    | ["janestreet"; r; "archive"; f] -> Some (strip_ext f)
    | _ -> parse_err () )
  | Some "gitlab.camlcity.org" | Some "download.camlcity.org" ->
      tag_of_last_path ()
  | Some "ocamlgraph.lri.fr" | Some "erratique.ch" ->
      tag_of_last_path ~prefix:"v" ()
  | _ ->
      Logs.debug (fun l ->
          l "Attempting to guess tag for %s from the final part of the URL"
            archive ) ;
      tag_of_last_path ()

let classify_package ~name ~version ~dev_repo ~archive () =
  Logs.debug (fun l -> l "dev-repo=%s" dev_repo) ;
  let err msg = (`Error msg, None) in
  let uri = Uri.of_string dev_repo in
  if List.mem name base_packages then (`Virtual, None)
  else
    match dev_repo with
    | "" -> (`Virtual, None)
    | dev_repo ->
      match archive with
      | None -> (`Virtual, None)
      | Some archive ->
          let tag = tag_from_archive archive in
          Logs.debug (fun l ->
              l "Mapped %s -> %s" archive
                (match tag with None -> "??" | Some v -> v) ) ;
          match List.assoc_opt dev_repo duniverse_forks with
          | Some repo -> (`Duniverse_fork repo, tag)
          | None ->
            match Uri.host uri with
            | Some "github.com" -> (
              match String.cuts ~empty:false ~sep:"/" (Uri.path uri) with
              | [user; repo] ->
                  let repo = strip_ext repo in
                  (`Github (user, repo), tag)
              | tl -> err "wierd github url" )
            | Some host -> (`Unknown host, tag)
            | None -> err "dev-repo without host"

let check_if_dune package =
  Cmd.get_opam_depends package
  >>| List.exists (fun l -> l = "jbuilder" || l = "dune")

let get_opam_info package =
  split_opam_name_and_version package
  >>= fun (name, version) ->
  Cmd.get_opam_dev_repo package
  >>= fun dev_repo ->
  Cmd.get_opam_archive_url package
  >>= fun archive ->
  check_if_dune package
  >>= fun is_dune ->
  let dev_repo, tag = classify_package ~name ~version ~dev_repo ~archive () in
  let is_dune =
    match (is_dune, dev_repo) with
    | true, `Duniverse_fork _ ->
        Logs.err (fun l ->
            l
              "%s.%s appears to be ported to Dune upstream. Do you still need \
               a Duniverse fork?"
              name version ) ;
        true
    | false, `Duniverse_fork _ ->
        Logs.info (fun l -> l "%s.%s is a Duniverse fork override" name version) ;
        true
    | is_dune, _ -> is_dune
  in
  Ok {name; version; dev_repo; tag; is_dune}

let package_is_valid {name; version; dev_repo} =
  match dev_repo with
  | `Error msg ->
      R.error_msg
        (Fmt.strf "Do not know how to deal with %s.%s: %s" name version msg)
  | `Unknown msg ->
      R.error_msg
        (Fmt.strf "Need a Duniverse fork for %s.%s: %s" name version msg)
  | _ -> R.ok ()

let rec check_packages_are_valid pkgs =
  Logs.info (fun l ->
      l "Checking that all dependencies are understood by the Duniverse" ) ;
  let rec fn = function
    | hd :: tl -> package_is_valid hd >>= fun () -> fn tl
    | [] -> R.ok ()
  in
  fn pkgs

let rec filter_duniverse_packages ~excludes pkgs =
  Logs.info (fun l ->
      l "Filtering out packages that are irrelevant to the Duniverse" ) ;
  let rec fn acc = function
    | hd :: tl ->
        let filter =
          List.mem hd.name excludes
          || hd.dev_repo = `Virtual
          || hd.name = "jbuilder" (* this is us *)
          || hd.name = "dune" (* this is us *)
          || hd.name = "ocamlbuild" (* build system *)
          || hd.name = "result" || hd.name = "uchar" || hd.name = "ocaml"
          || hd.name = "ocaml-base-compiler"
          || hd.name = "ocaml-variants" || hd.name = "ocamlfind"
          (* for now *)
        in
        if filter then fn acc tl else fn (hd :: acc) tl
    | [] -> Ok (List.rev acc)
  in
  fn [] pkgs

let init_duniverse roots ofile excludes () =
  Cmd.run_opam_package_deps roots
  >>= fun deps ->
  Logs.info (fun l -> l "Found %d opam dependencies" (List.length deps)) ;
  Logs.debug (fun l ->
      l "The dependencies for %s are: %s"
        (String.concat ~sep:" " roots)
        (String.concat ~sep:", " deps) ) ;
  Logs.info (fun l ->
      l "Querying opam for the dev repos and Dune compatibility" ) ;
  Cmd.map (fun p -> get_opam_info p) deps
  >>= fun pkgs ->
  check_packages_are_valid pkgs
  >>= fun () ->
  filter_duniverse_packages ~excludes pkgs
  >>= fun pkgs ->
  let is_dune_pkgs, not_dune_pkgs =
    List.partition (fun {is_dune} -> is_dune) pkgs
  in
  let num_dune = List.length is_dune_pkgs in
  let num_not_dune = List.length not_dune_pkgs in
  let num_total = List.length pkgs in
  let packages = {pkgs; roots; excludes} in
  if num_not_dune > 0 then
    Logs.err (fun l ->
        l
          "The good news is that %d/%d are Dune compatible.\n\
           The bad news is that you will have to fork these to the Duniverse \
           or port them upstream: %s"
          num_dune num_total
          (String.concat ~sep:", "
             (List.map
                (fun {name; version} -> Fmt.strf "%s.%s" name version)
                not_dune_pkgs)) )
  else
    Logs.info (fun l ->
        l "All %d packages are Dune compatible! It's a spicy miracle!"
          num_total ) ;
  Bos.OS.File.write ofile (Fmt.strf "%a\n" pp_packages packages)
  >>= fun () ->
  Fmt.pr "Written %a (%d packages)\n" Fpath.pp ofile num_total ;
  Ok ()
