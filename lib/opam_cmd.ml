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

let pp_plural = Fmt.using (function _ :: _ :: _ -> "s" | _ -> "") Fmt.string

let split_opam_name_and_version name =
  match String.cut ~sep:"." name with
  | None -> { name; version = None }
  | Some (name, version) -> { name; version = Some version }

let strip_ext fname =
  let open Fpath in
  let fname = v fname |> rem_ext in
  let fname = if has_ext "tar" fname then rem_ext fname else fname in
  to_string fname

let tag_from_archive archive =
  let uri = Uri.of_string archive in
  let path = String.cuts ~empty:false ~sep:"/" (Uri.path uri) in
  let parse_err () =
    Logs.err (fun l -> l "Unable to classify archive %s" archive);
    None
  in
  let tag_of_file ?(prefix = "") f =
    match strip_ext f |> String.cut ~rev:true ~sep:"-" with
    | None -> parse_err ()
    | Some (_n, v) -> Some (prefix ^ v)
  in
  let tag_of_last_path ?prefix () =
    match List.rev path with file :: _ -> tag_of_file ?prefix file | [] -> None
  in
  match Uri.scheme uri with
  | Some "git+http" | Some "git+https" | Some "git+ssh" | Some "git" -> (
      match String.cuts ~empty:false ~sep:"#" archive with
      | [ _repo; tag ] -> Some tag
      | _ -> Some "master" )
  | Some "git+file" -> None
  | _ -> (
      match Uri.host uri with
      | Some "github.com" -> (
          match path with
          | [ _u; _r; "releases"; "download"; v; _archive ] -> Some v
          | [ _u; _r; "archive"; archive ] -> Some (strip_ext archive)
          | [ _u; _r; "archive"; tag; _ ] -> Some tag
          | _ -> if Uri.scheme uri = Some "git+https" then None else parse_err () )
      | Some "ocaml.janestreet.com" -> (
          match path with
          | [ "ocaml-core"; _ver; "files"; f ] -> tag_of_file f
          | [ "janestreet"; _r; "releases"; "download"; v; _f ] -> Some v
          | [ "janestreet"; _r; "archive"; f ] -> Some (strip_ext f)
          | _ -> parse_err () )
      | Some "gitlab.camlcity.org" | Some "download.camlcity.org" -> tag_of_last_path ()
      | Some "gitlab.inria.fr" -> (
          match path with [ _u; _r; "repository"; v; _archive ] -> Some v | _ -> parse_err () )
      | Some "ocamlgraph.lri.fr" | Some "erratique.ch" -> tag_of_last_path ~prefix:"v" ()
      | _ ->
          Logs.info (fun l ->
              l "Attempting to guess tag for %s from the final part of the URL" archive);
          tag_of_last_path () )

let classify_from_url_src src =
  let src = match String.cut ~sep:"#" src with None -> src | Some (src, _) -> src in
  match src with
  | "" -> None
  | src -> (
      match Opam.Dev_repo.from_string src with
      | { vcs = Some Git; uri = dev_repo_uri } -> (
          match Uri.host dev_repo_uri with
          | Some _host -> Some (`Git (Uri.to_string dev_repo_uri))
          | None -> Some (`Error "url.src without host") )
      | { vcs = None | Some (Other _); _ } -> None )

let classify_from_dev_repo ~name src =
  match src with
  | None ->
      Logs.debug (fun l -> l "Mapped %s to a virtual package as it has a blank dev repo" name);
      `Virtual
  | Some src -> (
      match Opam.Dev_repo.from_string src with
      | { vcs = Some Git; uri = dev_repo_uri } -> (
          match Uri.host dev_repo_uri with
          | Some _host -> `Git (Uri.to_string dev_repo_uri)
          | None -> `Error "dev-repo without host" )
      | { vcs = None | Some (Other _); _ } -> `Error "dev-repo doesn't use git as a VCS" )

let get_git_url ~src ~dev_repo ~name =
  Logs.debug (fun l -> l "Parsing git url from url.src (%s)" name);
  match classify_from_url_src src with
  | Some x -> x
  | None ->
      Logs.debug (fun l -> l "Falling back to parsing git url from dev-repo (%s)" name);
      classify_from_dev_repo ~name dev_repo

let classify_package ~package ~dev_repo ~archive () =
  if List.mem package.name base_packages then (`Virtual, None)
  else
    match archive with
    | None ->
        Logs.debug (fun l -> l "Mapped %s to a virtual package as it has no archive" package.name);
        (`Virtual, None)
    | Some archive -> (
        match get_git_url ~src:archive ~dev_repo ~name:package.name with
        | `Git _ as kind ->
            let tag = tag_from_archive archive in
            Logs.debug (fun l ->
                l "Mapped %s -> %s" archive (match tag with None -> "??" | Some v -> v));
            (kind, tag)
        | x -> (x, None) )

let compute_depexts ~get_opam_path pkgs =
  let opam_depexts_of_pkg pkg =
    let opam_file = get_opam_path pkg in
    Bos.OS.File.read opam_file >>| fun opam_contents ->
    let opam = OpamFile.OPAM.read_from_string opam_contents in
    OpamFile.OPAM.depexts opam
  in
  let open Rresult.R in
  Exec.map opam_depexts_of_pkg pkgs >>| fun depexts ->
  List.flatten depexts |> List.sort_uniq Stdlib.compare

let get_opam_info ~get_opam_path packages =
  List.map
    (fun pkg ->
      let opam_file = get_opam_path pkg in
      Logs.info (fun l -> l "processing %a" Fpath.pp opam_file);
      let open OpamParserTypes in
      OpamParser.file (Fpath.to_string opam_file) |> fun { file_contents; _ } ->
      let archive =
        List.filter_map
          (function
            | Section (_, { section_kind = "url"; section_items; _ }) ->
                Some
                  (List.filter_map
                     (function Variable (_, "src", String (_, v)) -> Some v | _ -> None)
                     section_items)
            | _ -> None)
          file_contents
        |> List.flatten
        |> function
        | [ hd ] -> Some hd
        | _ -> None
      in
      let dev_repo =
        List.filter_map
          (function Variable (_, "dev-repo", String (_, v)) -> Some v | _ -> None)
          file_contents
        |> function
        | [ hd ] -> Some hd
        | _ -> None
      in
      let depends =
        List.filter_map
          (function
            | Variable (_, "depends", List (_, v)) ->
                Some
                  (List.filter_map
                     (function
                       | Option (_, String (_, v), _) -> Some v
                       | String (_, v) -> Some v
                       | _ -> None)
                     v)
            | _ -> None)
          file_contents
        |> List.flatten
      in
      let dev_repo, tag = classify_package ~package:pkg ~dev_repo ~archive () in
      let is_dune = List.exists (fun l -> l = "jbuilder" || l = "dune") depends in
      Logs.info (fun l ->
          l "Classified %a as %a with tag %a"
            Fmt.(styled `Yellow pp_package)
            pkg pp_repo dev_repo
            Fmt.(option string)
            tag);
      { package = pkg; dev_repo; tag; is_dune })
    packages
  |> fun v -> Ok v

(* TODO catch exceptions and turn to error *)

let filter_duniverse_packages pkgs =
  Logs.info (fun l ->
      l "%aFiltering out packages that are irrelevant to the Duniverse." pp_header header);
  let rec fn acc = function
    | hd :: tl ->
        let filter =
             List.mem hd.package.name Config.base_packages
          || hd.dev_repo = `Virtual
        in
        if filter then fn acc tl else fn (hd :: acc) tl
    | [] -> List.rev acc
  in
  fn [] pkgs

let calculate_opam ~packages ~get_opam_path ~local_opam_repo =
  let opam_files =
    List.map (fun pkg -> Fpath.to_string (get_opam_path pkg)) packages in
  Opam_solve.calculate ~opam_repo:local_opam_repo ~opam_files
  >>| List.map OpamPackage.to_string
  >>| List.map split_opam_name_and_version
  >>= fun deps ->
  Logs.app (fun l ->
      l "%aFound %a opam dependencies for %a." pp_header header
        Fmt.(styled `Green int)
        (List.length deps)
        Fmt.(list ~sep:(unit " ") Styled_pp.package)
        packages);
  Logs.info (fun l ->
      l "The dependencies for %a are: %a"
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        packages
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        deps);
  Logs.app (fun l ->
      l "%aQuerying opam database for their metadata and Dune compatibility." pp_header header);
  get_opam_info ~get_opam_path deps

type packages_stats = { total : int; dune : int; not_dune : entry list }

let packages_stats packages =
  filter_duniverse_packages packages |> fun packages ->
  let dune, not_dune = List.partition (fun { is_dune; _ } -> is_dune) packages in
  let dune = List.length dune in
  let total = List.length packages in
  { total; dune; not_dune }

let report_packages_stats packages =
  packages_stats packages |> fun packages_stats ->
  if packages_stats.dune < packages_stats.total then
    Logs.app (fun l ->
        l
          "%aThe good news is that %a/%a are Dune compatible.\n\
           The bad news is that you will have to fork these to the Duniverse or port them \
           upstream: %a.\n\
           In the meantime you can install them using `duniverse opam-install`." pp_header header
          Fmt.(styled `Green int)
          packages_stats.dune
          Fmt.(styled `Cyan int)
          packages_stats.total
          Fmt.(list ~sep:(unit ",@ ") Fmt.(styled `Red pp_entry))
          packages_stats.not_dune)
  else
    Logs.app (fun l ->
        l "%aAll %a opam packages are Dune compatible! It's a spicy miracle!" pp_header header
          Fmt.(styled `Green int)
          packages_stats.total)

let choose_root_packages ~local_packages =
  match local_packages with
  | [] ->
      R.error_msg
        "Cannot find any packages to vendor.\n\
         Either create some *.opam files in the local repository, or specify them manually via \
         'duniverse opam <packages>'."
  | local_packages ->
      Logs.app (fun l ->
          l "%aUsing locally scanned package%a '%a' as the root%a." pp_header header
            pp_plural local_packages
            Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
            local_packages pp_plural local_packages);
      Ok local_packages

let install_incompatible_packages yes repo =
  Repo.duniverse_file repo >>= fun file ->
  Logs.app (fun l ->
      l "%aGathering dune-incompatible packages from %a." pp_header header
        Fmt.(styled `Cyan Fpath.pp)
        file);
  Duniverse.load ~file >>= fun { deps = { opamverse; _ }; _ } ->
  match opamverse with
  | [] ->
      Logs.app (fun l -> l "%aGood news! There is no package to install!" pp_header header);
      Ok ()
  | opamverse ->
      Logs.app (fun l ->
          l "%aInstalling these packages with opam:\n%a" pp_header header
            Fmt.(list ~sep:sp Duniverse.Deps.Opam.pp)
            opamverse);
      Exec.run_opam_install ~yes opamverse
