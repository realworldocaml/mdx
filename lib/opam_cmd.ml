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
  | None -> { name; version = None }
  | Some (name, version) -> { name; version = Some version }

let strip_ext fname =
  let open Fpath in
  let fname = v fname |> rem_ext in
  let fname = if has_ext "tar" fname then rem_ext fname else fname in
  to_string fname

let find_local_opam_packages dir =
  Bos.OS.Dir.contents ~rel:true dir
  >>| List.filter (Fpath.has_ext ".opam")
  >>| List.map Fpath.rem_ext >>| List.map Fpath.to_string

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
    | Some "ocamlgraph.lri.fr" | Some "erratique.ch" -> tag_of_last_path ~prefix:"v" ()
    | _ ->
        Logs.info (fun l ->
            l "Attempting to guess tag for %s from the final part of the URL" archive );
        tag_of_last_path () )

let classify_from_url src =
  let src = match String.cut ~sep:"#" src with
  | None -> src
  | Some (src, _) -> src
  in
  match src with
  | "" -> None
  | src -> (
      match Opam.Dev_repo.from_string src with
      | { vcs = Some Git; uri = dev_repo_uri } -> (
        match Uri.host dev_repo_uri with
        | Some _host -> Some (`Git (Uri.to_string dev_repo_uri))
        | None -> Some (`Error ("url.src without host") ))
      | { vcs = None | Some (Other _); _ } -> None
    )


let classify_from_dev  ~name src = match src with
  | "" -> Logs.debug (fun l ->
    l "Mapped %s to a virtual package as it has a blank dev repo" name);
    `Virtual
  | src -> (
      match Opam.Dev_repo.from_string src with
      | { vcs = Some Git; uri = dev_repo_uri } -> (
        match Uri.host dev_repo_uri with
        | Some _host -> `Git (Uri.to_string dev_repo_uri)
        | None -> `Error ("dev-repo without host") )
      | { vcs = None | Some (Other _); _ } -> `Error "dev-repo doesn't use git as a VCS"
    )


let classify_package ~package ~dev_repo ~archive () =
  if List.mem package.name base_packages then (`Virtual, None)
  else
    match archive with
    | None -> Logs.debug (fun l -> l "Mapped %s to a virtual package as it has no archive" package.name);
      `Virtual, None
    | Some archive ->
      match (Stdune.Option.value (classify_from_url archive)
        ~default:(classify_from_dev ~name:package.name dev_repo)) with
      | `Git _ as kind ->
        let tag = tag_from_archive archive in
        Logs.debug (fun l ->
            l "Mapped %s -> %s" archive (match tag with None -> "??" | Some v -> v) );
        kind, tag
      | x -> x, None

(* Fetch and parse an opam field from an Opam_show_result.t*)
let extract_opam_value ~field ~package data =
  let v = Opam_show_result.get ~field ~package data in
  match v with
  | None -> None
  | Some str ->
      let res =
        try Ok (OpamParser.value_from_string str "")
        with OpamLexer.Error _ ->
          let error_message = Printf.sprintf "Failed to parse %S" str in
          Error (`Msg error_message)
      in
      Some res

let parse_string ~field ~package data =
  let open OpamParserTypes in
  match extract_opam_value ~field ~package data with
  | Some (Ok (String (_, v))) -> Ok v
  | Some (Ok (List (_, []))) -> Ok ""
  | None -> Ok ""
  | _ ->
      R.error_msg
        (Fmt.strf "Unable to parse opam string.\nTry `opam show --normalise -f %s: %s`" field
           package)

let parse_dev_repo = parse_string ~field:"dev-repo:"

let parse_archive_url ~package data =
  parse_string ~field:"url.src:" ~package data >>= function "" -> Ok None | uri -> Ok (Some uri)

let parse_opam_depends ~package data =
  let open OpamParserTypes in
  match extract_opam_value ~field:"depends:" ~package data with
  | Some (Ok (List (_, vs))) ->
      let ss =
        List.fold_left
          (fun acc -> function Option (_, String (_, v), _) | String (_, v) -> v :: acc | _ -> acc
            )
          [] vs
      in
      Logs.debug (fun l -> l "Depends for %s: %s" package (String.concat ~sep:" " ss));
      Ok ss
  | None -> Ok []
  | _ ->
      R.error_msg
        (Fmt.strf
           "Unable to parse opam depends for %s\n\
            Try `opam show --normalise -f depends: %s` manually"
           package package)

let get_opam_info ~root ~pins packages =
  let fields = [ "name"; "dev-repo:"; "url.src:"; "depends:" ] in
  Exec.run_opam_show ~root ~packages ~fields >>= fun lines ->
  Opam_show_result.make lines >>= fun data ->
  let packages =
    List.map
      (fun pkg ->
        let name = pkg.name in
        let archive = parse_archive_url ~package:name data in
        let dev_repo = parse_dev_repo ~package:name data in
        let depends = parse_opam_depends ~package:name data in
        dev_repo >>= fun dev_repo ->
        archive >>= fun archive ->
        depends >>| fun depends ->
        let dev_repo, tag = classify_package ~package:pkg ~dev_repo ~archive () in
        let is_dune = List.exists (fun l -> l = "jbuilder" || l = "dune") depends in
        Logs.info (fun l ->
            l "Classified %a as %a with tag %a"
              Fmt.(styled `Yellow pp_package)
              pkg pp_repo dev_repo
              Fmt.(option string)
              tag );
        let tag =
          match List.find_opt (fun { pin; _ } -> pkg.name = pin) pins with
          | Some { tag; _ } -> tag
          | None -> tag
        in
        { package = pkg; dev_repo; tag; is_dune } )
      packages
  in
  List.fold_left
    (fun lst elem -> lst >>= fun lst -> elem >>| fun elem -> elem :: lst)
    (Ok []) packages

let filter_duniverse_packages ~excludes pkgs =
  Logs.app (fun l ->
      l "%aFiltering out packages that are irrelevant to the Duniverse." pp_header header );
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

let calculate_opam ~root ~config =
  let { Duniverse.Config.root_packages; pins; excludes; _ } = config in
  Exec.run_opam_package_deps ~root (List.map string_of_package root_packages)
  >>| List.map split_opam_name_and_version
  >>| List.map (fun p ->
          if List.exists (fun { pin; _ } -> p.name = pin) pins then { p with version = Some "dev" }
          else p )
  >>= fun deps ->
  Logs.app (fun l ->
      l "%aFound %a opam dependencies." pp_header header Fmt.(styled `Green int) (List.length deps)
  );
  Logs.info (fun l ->
      l "The dependencies for %a are: %a"
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        root_packages
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        deps );
  Logs.app (fun l ->
      l "%aQuerying local opam switch for their metadata and Dune compatibility." pp_header header
  );
  get_opam_info ~root ~pins deps >>= filter_duniverse_packages ~excludes

type packages_stats = { total : int; dune : int; not_dune : entry list }

let packages_stats packages =
  let dune, not_dune = List.partition (fun { is_dune; _ } -> is_dune) packages in
  let dune = List.length dune in
  let total = List.length packages in
  { total; dune; not_dune }

let report_packages_stats packages_stats =
  if packages_stats.dune < packages_stats.total then
    Logs.app (fun l ->
        l
          "%aThe good news is that %a/%a are Dune compatible.\n\
           The bad news is that you will have to fork these to the Duniverse or port them \
           upstream: %a.\n\
           In the meantime you can install them using `duniverse opam-install`."
          pp_header header
          Fmt.(styled `Green int)
          packages_stats.dune
          Fmt.(styled `Cyan int)
          packages_stats.total
          Fmt.(list ~sep:(unit ",@ ") Fmt.(styled `Red pp_entry))
          packages_stats.not_dune )
  else
    Logs.app (fun l ->
        l "%aAll %a opam packages are Dune compatible! It's a spicy miracle!" pp_header header
          Fmt.(styled `Green int)
          packages_stats.total )

let init_opam ~root ~remotes () =
  let open Types.Opam.Remote in
  let remotes = List.mapi (fun i url -> { name = Fmt.strf "remote%d" i; url }) remotes in
  Exec.init_opam_and_remotes ~root ~remotes ()

let choose_root_packages ~explicit_root_packages ~local_packages =
  match (explicit_root_packages, local_packages) with
  | [], [] ->
      R.error_msg
        "Cannot find any packages to vendor.\n\
         Either create some *.opam files in the local repository, or specify them manually via \
         'duniverse opam <packages>'."
  | [], local_packages ->
      Logs.app (fun l ->
          l "%aUsing locally scanned packages '%a' as the roots." pp_header header
            Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
            local_packages );
      Ok local_packages
  | explicit_root_packages, [] ->
      Logs.app (fun l ->
          l "%aUsing command-line specified packages '%a' as the roots." pp_header header
            Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
            explicit_root_packages );
      Ok explicit_root_packages
  | explicit_root_packages, local_packages ->
      Logs.app (fun l ->
          l
            "%aUsing command-line specified packages '%a' as the roots.\n\
             Ignoring the locally found packages %a unless they are explicitly specified on the \
             command line."
            pp_header header
            Fmt.(list ~sep:(unit ",@ ") (styled `Yellow string))
            explicit_root_packages
            Fmt.(list ~sep:(unit ",@ ") string)
            local_packages );
      Ok explicit_root_packages

let install_incompatible_packages yes repo =
  Logs.app (fun l ->
      l "%aGathering dune-incompatible packages from %a." pp_header header
        Fmt.(styled `Cyan Fpath.pp)
        Config.duniverse_file );
  let file = Fpath.(repo // Config.duniverse_file) in
  Duniverse.load ~file >>= fun { deps = { opamverse; _ }; _ } ->
  match opamverse with
  | [] ->
      Logs.app (fun l -> l "%aGood news! There is no package to install!" pp_header header);
      Ok ()
  | opamverse ->
      Logs.app (fun l ->
          l "%aInstalling these packages with opam:\n%a" pp_header header
            Fmt.(list ~sep:sp Duniverse.Deps.Opam.pp)
            opamverse );
      Exec.run_opam_install ~yes opamverse
