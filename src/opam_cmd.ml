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

let split_opam_name_and_version name =
  match String.cut ~sep:"." name with
  | None -> {name; version= None}
  | Some (name, version) -> {name; version= Some version}

let strip_ext fname =
  let open Fpath in
  let fname = v fname |> rem_ext in
  let fname = if has_ext "tar" fname then rem_ext fname else fname in
  to_string fname

let find_local_opam_packages dir =
  Bos.OS.Dir.contents ~rel:true dir
  >>| List.filter (Fpath.has_ext ".opam")
  >>| List.map Fpath.rem_ext >>| List.map Fpath.to_string
  >>| fun pkgs ->
  if List.length pkgs > 0 then
    Logs.app (fun l ->
        l "Filtering out local opam packages: %a."
          Fmt.(list ~sep:(unit ",@ ") string)
          pkgs ) ;
  pkgs

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
  match Uri.scheme uri with
  | Some "git+http" | Some "git+https" ->
      (* TODO handle branches for pins *)
      Some "master"
  | Some "git+file" -> None
  | _ ->
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
        Logs.info (fun l ->
            l "Attempting to guess tag for %s from the final part of the URL"
              archive ) ;
        tag_of_last_path ()

let classify_package ~package ~dev_repo ~archive () =
  Logs.debug (fun l -> l "dev-repo=%s" dev_repo) ;
  let err msg = (`Error msg, None) in
  let uri = Uri.of_string dev_repo in
  if List.mem package.name base_packages then (`Virtual, None)
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

let check_if_dune ~repo package =
  Cmd.get_opam_depends ~repo (string_of_package package)
  >>| List.exists (fun l -> l = "jbuilder" || l = "dune")

let get_opam_info ~repo ~pins package =
  Cmd.get_opam_dev_repo ~repo (string_of_package package)
  >>= fun dev_repo ->
  Cmd.get_opam_archive_url ~repo (string_of_package package)
  >>= fun archive ->
  check_if_dune ~repo package
  >>= fun is_dune ->
  let dev_repo, tag = classify_package ~package ~dev_repo ~archive () in
  let tag = if List.mem package.name pins then Some "master" else tag in
  let is_dune =
    match (is_dune, dev_repo) with
    | true, `Duniverse_fork _ ->
        Logs.err (fun l ->
            l
              "%a appears to be ported to Dune upstream. Do you still need a \
               Duniverse fork?"
              pp_package package ) ;
        true
    | false, `Duniverse_fork _ ->
        Logs.info (fun l ->
            l "%a is a Duniverse fork override" pp_package package ) ;
        true
    | is_dune, _ -> is_dune
  in
  Ok {package; dev_repo; tag; is_dune}

let package_is_valid {package; dev_repo} =
  match dev_repo with
  | `Error msg ->
      R.error_msg
        (Fmt.strf "Do not know how to deal with %a: %s" pp_package package msg)
  | `Unknown msg ->
      R.error_msg
        (Fmt.strf "Need a Duniverse fork for %a: %s" pp_package package msg)
  | _ -> R.ok ()

let rec check_packages_are_valid pkgs =
  Logs.app (fun l ->
      l "Checking that all dependencies are understood by the Duniverse." ) ;
  let rec fn = function
    | hd :: tl -> package_is_valid hd >>= fun () -> fn tl
    | [] -> R.ok ()
  in
  fn pkgs

let rec filter_duniverse_packages ~excludes pkgs =
  Logs.app (fun l ->
      l "Filtering out packages that are irrelevant to the Duniverse." ) ;
  let rec fn acc = function
    | hd :: tl ->
        let filter =
          List.exists (fun e -> e.name = hd.package.name) excludes
          || List.mem hd.package.name Config.base_packages
          || hd.dev_repo = `Virtual
        in
        if filter then fn acc tl else fn (hd :: acc) tl
    | [] -> Ok (List.rev acc)
  in
  fn [] pkgs

let calculate_duniverse ~repo file =
  load file
  >>= fun {roots; excludes; pkgs; pins; opam_switch} ->
  Cmd.run_opam_package_deps ~repo (List.map string_of_package roots)
  >>| List.map split_opam_name_and_version
  >>| List.map (fun p ->
          if List.mem p.name pins then {p with version= Some "dev"} else p )
  >>= fun deps ->
  Logs.app (fun l -> l "Found %d opam dependencies." (List.length deps)) ;
  Logs.debug (fun l ->
      l "The dependencies for %a are: %a"
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        roots
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        deps ) ;
  Logs.info (fun l ->
      l "Querying opam for the dev repos and Dune compatibility" ) ;
  Cmd.map (fun p -> get_opam_info ~repo ~pins p) deps
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
  let t = {pkgs; roots; excludes; pins; opam_switch} in
  if num_not_dune > 0 then
    Logs.app (fun l ->
        l
          "The good news is that %d/%d are Dune compatible.\n\
           The bad news is that you will have to fork these to the Duniverse \
           or port them upstream: %a."
          num_dune num_total
          Fmt.(list ~sep:(unit ",@ ") pp_entry)
          not_dune_pkgs )
  else
    Logs.app (fun l ->
        l "All %d packages are Dune compatible! It's a spicy miracle!"
          num_total ) ;
  save file t
  >>= fun () ->
  Logs.app (fun l -> l "Written %a (%d packages)." Fpath.pp file num_total) ;
  Ok ()

let init_duniverse repo roots excludes pins opam_switch () =
  let file = Fpath.(repo // Config.opam_lockfile) in
  Bos.OS.Dir.create Fpath.(repo // duniverse_dir)
  >>= fun _ ->
  Cmd.init_local_opam_switch ~opam_switch ~repo ()
  >>= fun () ->
  Cmd.(iter (add_opam_dev_pin ~repo) pins)
  >>= fun () ->
  find_local_opam_packages repo
  >>= fun locals ->
  Cmd.(iter (add_opam_local_pin ~repo) locals)
  >>= fun () ->
  let roots =
    match (roots, locals) with
    | [], [] ->
        Logs.err (fun l ->
            l
              "Cannot find any packages to vendor.\n\
               Either create some *.opam files in the local repository, or \
               specify them manually via 'duniverse opam <packages'." ) ;
        exit 1
    | [], locals ->
        Logs.info (fun l ->
            l "Using locally scanned packages '%a' as the roots."
              Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
              locals ) ;
        locals
    | roots, [] ->
        Logs.info (fun l ->
            l "Using command-line specified packages '%a' as the roots."
              Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
              roots ) ;
        roots
    | roots, locals ->
        Logs.info (fun l ->
            l
              "Using command-line specified packages '%a' as the roots.\n\
               Ignoring the locally found packages %a unless they are \
               explicitly specified on the command line."
              Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
              roots
              Fmt.(list ~sep:(unit ",@ ") string)
              locals ) ;
        roots
  in
  let excludes =
    List.map split_opam_name_and_version (locals @ excludes) |> sort_uniq
  in
  let roots = List.map split_opam_name_and_version roots |> sort_uniq in
  save file {pkgs= []; roots; excludes; pins; opam_switch}
  >>= fun () -> calculate_duniverse ~repo file
