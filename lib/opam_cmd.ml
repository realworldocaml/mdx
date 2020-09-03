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

let depends_on_dune (formula : OpamTypes.filtered_formula) =
  let dune = OpamPackage.Name.of_string "dune" in
  let jbuilder = OpamPackage.Name.of_string "jbuilder" in
  let is_duneish name =
    let eq n n' = OpamPackage.Name.compare n n' = 0 in
    eq dune name || eq jbuilder name
  in
  OpamFormula.fold_left (fun acc (name, _) -> acc || is_duneish name) false formula

let get_opam_info ~switch_state package =
  let opam_pkg =  Types.Opam.package_to_opam package in
  let opam_file = OpamSwitchState.opam switch_state opam_pkg in
  let archive =
    Stdune.Option.map
      ~f:(fun x -> OpamUrl.to_string (OpamFile.URL.url x))
    (OpamFile.OPAM.url opam_file)
  in
  let dev_repo = Stdune.Option.map ~f:OpamUrl.to_string (OpamFile.OPAM.dev_repo opam_file) in
  let depends = OpamFile.OPAM.depends opam_file in
  let dev_repo, tag = classify_package ~package ~dev_repo ~archive () in
  let is_dune = depends_on_dune depends in
  Logs.info (fun l ->
      l "Classified %a as %a with tag %a"
        Fmt.(styled `Yellow pp_package)
        package pp_repo dev_repo
        Fmt.(option string)
        tag);
  { package; dev_repo; tag; is_dune }

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

let read_opam fpath =
  let filename = OpamFile.make (OpamFilename.of_string (Fpath.to_string fpath)) in
  Bos.OS.File.with_ic fpath
    (fun ic () -> OpamFile.OPAM.read_from_channel ~filename ic)
    ()

let local_paths_to_opam_map local_paths =
  let open Rresult.R.Infix in
  let bindings = String.Map.bindings local_paths in
  Stdext.Result.List.map bindings
    ~f:(fun (name, path) ->
        read_opam path >>| fun opam_file ->
        (OpamPackage.Name.of_string name, opam_file))
  >>| OpamPackage.Name.Map.of_list

let calculate_opam ~local_paths ~local_packages switch_state =
  let open Rresult.R.Infix in
  local_paths_to_opam_map local_paths >>= fun local_packages_opam ->
  Opam_solve.calculate ~local_packages:local_packages_opam switch_state
  >>| List.map Types.Opam.package_from_opam
  >>= fun deps ->
  let packages = List.map (fun name -> {Types.Opam.name; version = None}) local_packages in
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
  Ok (List.map (get_opam_info ~switch_state) deps)

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
