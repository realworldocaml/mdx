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

let pp_header = Fmt.(styled `Blue string)

let header = "==> "

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
        l "%aFiltering out local opam packages: %a." pp_header header
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
  let tag_of_file ?(prefix = "") f =
    match strip_ext f |> String.cut ~rev:true ~sep:"-" with
    | None -> parse_err ()
    | Some (_n, v) -> Some (prefix ^ v)
  in
  let tag_of_last_path ?prefix () =
    List.rev path |> List.hd |> tag_of_file ?prefix
  in
  match Uri.scheme uri with
  | Some "git+http" | Some "git+https"
  | Some "git+ssh" | Some "git" -> (
    match String.cuts ~empty:false ~sep:"#" archive with
    | [_repo; tag] -> Some tag
    | _ -> Some "master")
  | Some "git+file" -> None
  | _ -> (
    match Uri.host uri with
    | Some "github.com" -> (
      match path with
      | [_u; _r; "releases"; "download"; v; _archive] -> Some v
      | [_u; _r; "archive"; archive] -> Some (strip_ext archive)
      | [_u; _r; "archive"; tag; _] -> Some tag
      | _ -> if Uri.scheme uri = Some "git+https" then None else parse_err () )
    | Some "ocaml.janestreet.com" -> (
      match path with
      | ["ocaml-core"; _ver; "files"; f] -> tag_of_file f
      | ["janestreet"; _r; "releases"; "download"; v; _f] -> Some v
      | ["janestreet"; _r; "archive"; f] -> Some (strip_ext f)
      | _ -> parse_err () )
    | Some "gitlab.camlcity.org" | Some "download.camlcity.org" ->
        tag_of_last_path ()
    | Some "ocamlgraph.lri.fr" | Some "erratique.ch" ->
        tag_of_last_path ~prefix:"v" ()
    | _ ->
        Logs.info (fun l ->
            l "Attempting to guess tag for %s from the final part of the URL"
              archive ) ;
        tag_of_last_path () )

let classify_package ~package ~dev_repo ~archive ~pins () =
  let err msg = (`Error msg, None) in
  if List.mem package.name base_packages then (`Virtual, None)
  else
    let dev_repo =
      match List.find_opt (fun {pin; _} -> package.name = pin) pins with
      | Some {url= Some url; _} -> url
      | _ -> dev_repo
    in
    match dev_repo with
    | "" ->
        Logs.debug (fun l ->
            l "Mapped %s to a virtual package as it has a blank dev repo"
              package.name ) ;
        (`Virtual, None)
    | dev_repo -> (
      match archive with
      | None ->
          Logs.debug (fun l ->
              l "Mapped %s to a virtual package as it has no archive"
                package.name ) ;
          (`Virtual, None)
      | Some archive -> (
          let uri = Uri.of_string dev_repo in
          let tag = tag_from_archive archive in
          Logs.debug (fun l ->
              l "Mapped %s -> %s" archive
                (match tag with None -> "??" | Some v -> v) ) ;
          match Uri.host uri with
          | Some "github.com" -> (
            match String.cuts ~empty:false ~sep:"/" (Uri.path uri) with
            | [user; repo] ->
                let repo = strip_ext repo in
                (`Github (user, repo), tag)
            | _ -> err "weird github url" )
          | Some host -> (
            match String.is_prefix ~affix:"git" archive with
            | true -> (
                let base_repo = String.cuts ~empty:false ~sep:"#" archive |> List.hd in
                (`Git base_repo, tag) )
            | false -> (`Unknown host, tag) )
          | None -> err "dev-repo without host" ) )

let check_if_dune ~root package =
  Exec.get_opam_depends ~root (string_of_package package)
  >>| List.exists (fun l -> l = "jbuilder" || l = "dune")

let get_opam_info ~root ~pins package =
  Exec.get_opam_dev_repo ~root (string_of_package package)
  >>= fun dev_repo ->
  Exec.get_opam_archive_url ~root (string_of_package package)
  >>= fun archive ->
  let dev_repo, tag = classify_package ~package ~dev_repo ~archive ~pins () in
  check_if_dune ~root package
  >>= fun is_dune ->
  Logs.info (fun l ->
      l "Classified %a as %a with tag %a"
        Fmt.(styled `Yellow pp_package)
        package pp_repo dev_repo
        Fmt.(option string)
        tag ) ;
  let tag =
    match List.find_opt (fun {pin; _} -> package.name = pin) pins with
    | Some {tag; _} -> tag
    | None -> tag
  in
  Ok {package; dev_repo; tag; is_dune}

let package_is_valid {package; dev_repo; _} =
  match dev_repo with
  | `Error msg ->
      R.error_msg
        (Fmt.strf "Do not know how to deal with %a: %s" pp_package package msg)
  | `Unknown msg ->
      R.error_msg
        (Fmt.strf "Need a Duniverse fork for %a: %s" pp_package package msg)
  | _ -> R.ok ()

let check_packages_are_valid pkgs =
  Logs.app (fun l ->
      l "%aChecking that all dependencies are understood by the Duniverse."
        pp_header header ) ;
  let rec fn = function
    | hd :: tl -> package_is_valid hd >>= fun () -> fn tl
    | [] -> R.ok ()
  in
  fn pkgs

let filter_duniverse_packages ~excludes pkgs =
  Logs.app (fun l ->
      l "%aFiltering out packages that are irrelevant to the Duniverse."
        pp_header header ) ;
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

let calculate_duniverse ~root file =
  load file
  >>= fun {roots; excludes; pins; opam_switch; branch; remotes; _} ->
  Exec.run_opam_package_deps ~root (List.map string_of_package roots)
  >>| List.map split_opam_name_and_version
  >>| List.map (fun p ->
          if List.exists (fun {pin; _} -> p.name = pin) pins then
            {p with version= Some "dev"}
          else p )
  >>= fun deps ->
  Logs.app (fun l ->
      l "%aFound %a opam dependencies." pp_header header
        Fmt.(styled `Green int)
        (List.length deps) ) ;
  Logs.info (fun l ->
      l "The dependencies for %a are: %a"
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        roots
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        deps ) ;
  Logs.app (fun l ->
      l
        "%aQuerying local opam switch for their metadata and Dune \
         compatibility."
        pp_header header ) ;
  Exec.map (fun p -> get_opam_info ~root ~pins p) deps
  >>= fun pkgs ->
  check_packages_are_valid pkgs
  >>= fun () ->
  filter_duniverse_packages ~excludes pkgs
  >>= fun pkgs ->
  let is_dune_pkgs, not_dune_pkgs =
    List.partition (fun {is_dune; _} -> is_dune) pkgs
  in
  let num_dune = List.length is_dune_pkgs in
  let num_not_dune = List.length not_dune_pkgs in
  let num_total = List.length pkgs in
  let t = {pkgs; roots; excludes; pins; opam_switch; branch; remotes} in
  if num_not_dune > 0 then
    Logs.app (fun l ->
        l
          "%aThe good news is that %a/%a are Dune compatible.\n\
           The bad news is that you will have to fork these to the Duniverse \
           or port them upstream: %a."
          pp_header header
          Fmt.(styled `Green int)
          num_dune
          Fmt.(styled `Cyan int)
          num_total
          Fmt.(list ~sep:(unit ",@ ") Fmt.(styled `Red pp_entry))
          not_dune_pkgs )
  else
    Logs.app (fun l ->
        l "%aAll %a opam packages are Dune compatible! It's a spicy miracle!"
          pp_header header
          Fmt.(styled `Green int)
          num_total ) ;
  save file t
  >>= fun () ->
  Logs.app (fun l ->
      l "%aWritten %a opam packages to %a." pp_header header
        Fmt.(styled `Green int)
        num_total
        Fmt.(styled `Cyan Fpath.pp)
        file ) ;
  Ok ()

let init_opam ~root ~remotes () =
  let open Types.Opam.Remote in
  let dune_overlays = {name = "dune-overlays"; url = Config.duniverse_overlays_repo} in
  let user_specified_remotes =
    List.mapi (fun i url -> {name = Fmt.strf "remote%d" i; url}) remotes
  in
  Exec.init_opam_and_remotes ~root ~remotes:(dune_overlays::user_specified_remotes) ()

let init_duniverse repo branch roots excludes pins compiler remotes () =
  Logs.app (fun l ->
      l "%aCalculating Duniverse on the %a branch." pp_header header
        Fmt.(styled `Cyan string)
        branch ) ;
  let file = Fpath.(repo // Config.opam_lockfile) in
  Bos.OS.Dir.tmp ".duniverse-opam-root-%s"
  >>= fun root ->
  Bos.OS.Dir.create Fpath.(repo // duniverse_dir)
  >>= fun _ ->
  init_opam ~root ~remotes ()
  >>= fun () ->
  Exec.(iter (add_opam_dev_pin ~root) pins)
  >>= fun () ->
  find_local_opam_packages repo
  >>= fun locals ->
  Exec.(iter (add_opam_local_pin ~root) locals)
  >>= fun () ->
  let roots =
    match (roots, locals) with
    | [], [] ->
        Logs.err (fun l ->
            l
              "Cannot find any packages to vendor.\n\
               Either create some *.opam files in the local repository, or \
               specify them manually via 'duniverse opam <packages>'." ) ;
        exit 1
    | [], locals ->
        Logs.app (fun l ->
            l "%aUsing locally scanned packages '%a' as the roots." pp_header
              header
              Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
              locals ) ;
        locals
    | roots, [] ->
        Logs.app (fun l ->
            l "%aUsing command-line specified packages '%a' as the roots."
              pp_header header
              Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
              roots ) ;
        roots
    | roots, locals ->
        Logs.app (fun l ->
            l
              "%aUsing command-line specified packages '%a' as the roots.\n\
               Ignoring the locally found packages %a unless they are \
               explicitly specified on the command line."
              pp_header header
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
  save file {pkgs= []; roots; excludes; pins; opam_switch = compiler; branch; remotes}
  >>= fun () -> calculate_duniverse ~root file
