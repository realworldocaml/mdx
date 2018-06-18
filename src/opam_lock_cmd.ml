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

let split_opam_name_and_version b =
  match String.cut ~sep:"." b with
  | None -> R.error_msg (Fmt.strf "Unable to find version string in %s" b)
  | Some r -> Ok r

let strip_ext fname =
  let open Fpath in
  let fname = v fname |> rem_ext in
  let fname = if has_ext "tar" fname then rem_ext fname else fname in
  to_string fname

let duniverse_forks =
  [ ("https://github.com/hannesm/duration", "duration")
  ; ("https://github.com/backtracking/ocaml-hashcons", "ocaml-hashcons") ]

let known_duniverse_domains =
  ["gitlab.camlcity.org"; (* ocamlfind *) "erratique.ch" (* dbuenzli *)
  ]

(* Classify the dev-repo. TODO: extend the classifier to look online in
   https://github.com/dune-universe *)
let classify_dev_repo ~dev_repo ~archive () =
  if archive = None then `Virtual
  else if dev_repo = "" then `None
  else
    let uri = Uri.of_string dev_repo in
    match Uri.host uri with
    | Some "github.com" -> (
      match String.cuts ~empty:false ~sep:"/" (Uri.path uri) with
      | [user; repo] ->
          let repo = strip_ext repo in
          `Github (user, repo)
      | tl -> `Error "wierd github url" )
    | Some domain when List.mem domain known_duniverse_domains -> (
      match String.cuts ~empty:false ~sep:"/" (Uri.path uri) with
      | [user; repo] ->
          let repo = strip_ext repo in
          `Duniverse_fork repo
      | _ -> `Error "weird gitlab camlcity url" )
    | Some host -> `Unknown host
    | None -> `Error "wierd unknown url with no path"

let classify_pkg_archive_into_tag ~name ~version ~dev_repo ~archive =
  match dev_repo with
  | `Github ("janestreet", repo) -> (
      (* JS archives sit on their server *)
      let parse_err () =
        Logs.err (fun l ->
            l "Unable to classify Jane Street archive %s.%s: %s" name version
              (Uri.to_string archive) ) ;
        exit 1
      in
      match String.cuts ~empty:false ~sep:"/" (Uri.path archive) with
      | ["ocaml-core"; ver; "files"; archive] -> (
        match strip_ext archive |> String.cut ~rev:true ~sep:"-" with
        | None -> parse_err ()
        | Some (n, v) -> Some v )
      | ["janestreet"; r; "releases"; "download"; v; archive] -> Some v
      | ["janestreet"; r; "archive"; archive] -> Some (strip_ext archive)
      | _ -> parse_err () )
  | `Github (user, repo) -> (
    match String.cuts ~empty:false ~sep:"/" (Uri.path archive) with
    | [u; r; "releases"; "download"; v; archive] -> Some v
    | [u; r; "archive"; archive] -> Some (strip_ext archive)
    | _ ->
      match Uri.scheme archive with
      | Some "git+https" -> None
      | _ ->
          Logs.err (fun l ->
              l "Unable to classify GitHub archive %s.%s: %s" name version
                (Uri.to_string archive) ) ;
          None )
  | _ -> None

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
  let dev_repo = classify_dev_repo ~dev_repo ~archive () in
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
  let tag =
    match archive with
    | None -> None
    | Some archive ->
        classify_pkg_archive_into_tag ~name ~version ~dev_repo ~archive
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

let rec filter_duniverse_packages pkgs =
  Logs.info (fun l ->
      l "Filtering out packages that are irrelevant to the Duniverse" ) ;
  let rec fn acc = function
    | hd :: tl ->
        let filter =
          hd.dev_repo = `Virtual
          || hd.name = "jbuilder" (* this is us *)
          || hd.name = "dune" (* this is us *)
          || hd.name = "ocamlbuild" (* build system *)
          || hd.name = "result" || hd.name = "uchar"
          || hd.name = "ocaml-base-compiler"
          || hd.name = "ocaml-variants" || hd.name = "ocamlfind"
          (* for now *)
        in
        if filter then fn acc tl else fn (hd :: acc) tl
    | [] -> Ok (List.rev acc)
  in
  fn [] pkgs

let init_duniverse package ofile () =
  Cmd.run_opam_package_deps package
  >>= fun deps ->
  Logs.info (fun l -> l "Found %d opam dependencies" (List.length deps)) ;
  Logs.debug (fun l ->
      l "The dependencies for %s are: %s" package
        (String.concat ~sep:", " deps) ) ;
  Logs.info (fun l ->
      l "Querying opam for the dev repos and Dune compatibility" ) ;
  Cmd.map (fun p -> get_opam_info p) deps
  >>= fun pkgs ->
  check_packages_are_valid pkgs
  >>= fun () ->
  filter_duniverse_packages pkgs
  >>= fun pkgs ->
  let is_dune_pkgs, not_dune_pkgs =
    List.partition (fun {is_dune} -> is_dune) pkgs
  in
  let num_dune = List.length is_dune_pkgs in
  let num_not_dune = List.length not_dune_pkgs in
  let num_total = List.length pkgs in
  let packages = {pkgs} in
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
  Fmt.pr "Written %a (%d packages)\n" pp_packages packages num_total ;
  Ok ()
