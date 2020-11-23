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

open Import
open Types.Opam

let get_opam_info ~switch_state pkg =
  let opam_file = OpamSwitchState.opam switch_state pkg in
  Opam.Package_summary.from_opam ~pkg opam_file

(* TODO catch exceptions and turn to error *)

let read_opam fpath =
  let filename = OpamFile.make (OpamFilename.of_string (Fpath.to_string fpath)) in
  Bos.OS.File.with_ic fpath (fun ic () -> OpamFile.OPAM.read_from_channel ~filename ic) ()

let local_paths_to_opam_map local_paths =
  let open Result.O in
  let bindings = String.Map.bindings local_paths in
  Result.List.map bindings ~f:(fun (name, (version, path)) ->
      read_opam path >>| fun opam_file ->
      let name = OpamPackage.Name.of_string name in
      let version =
        OpamPackage.Version.of_string (Option.value ~default:Types.Opam.default_version version)
      in
      (name, (version, opam_file)))
  >>| OpamPackage.Name.Map.of_list

let calculate_opam ~build_only ~local_paths ~local_packages switch_state =
  let open Rresult.R.Infix in
  local_paths_to_opam_map local_paths >>= fun local_packages_opam ->
  Opam_solve.calculate ~build_only ~local_packages:local_packages_opam switch_state >>= fun deps ->
  Logs.app (fun l ->
      l "%aFound %a opam dependencies for %a." Pp.Styled.header ()
        Fmt.(styled `Green int)
        (List.length deps)
        Fmt.(list ~sep:(unit " ") Pp.Styled.package)
        local_packages);
  Logs.info (fun l ->
      l "The dependencies for %a are: %a"
        Fmt.(list ~sep:(unit ",@ ") pp_package)
        local_packages
        Fmt.(list ~sep:(unit ",@ ") Opam.Pp.package)
        deps);
  Logs.app (fun l ->
      l "%aQuerying opam database for their metadata and Dune compatibility." Pp.Styled.header ());
  Ok (List.map ~f:(get_opam_info ~switch_state) deps)

type packages_stats = { total : int; dune : int; not_dune : package list }

let filter_duniverse_packages package_summaries =
  Logs.info (fun l ->
      l "%aFiltering out packages that are irrelevant to the Duniverse." Pp.Styled.header ());
  List.filter
    ~f:(fun ps -> not Opam.Package_summary.(is_base_package ps || is_virtual ps))
    package_summaries

let packages_stats package_summaries =
  filter_duniverse_packages package_summaries |> fun packages ->
  let dune, not_dune = List.partition ~f:Opam.Package_summary.uses_dune packages in
  let dune = List.length dune in
  let total = List.length packages in
  let not_dune =
    List.map
      ~f:(fun { Opam.Package_summary.name; version; _ } ->
        { Types.Opam.name; version = Some version })
      not_dune
  in
  { total; dune; not_dune }

let report_packages_stats packages =
  packages_stats packages |> fun packages_stats ->
  if packages_stats.dune < packages_stats.total then
    Logs.app (fun l ->
        l
          "%aThe good news is that %a/%a are Dune compatible.\n\
           The bad news is that you will have to fork these to the Duniverse or port them \
           upstream: %a.\n\
           In the meantime you can install them using `duniverse opam-install`." Pp.Styled.header ()
          Fmt.(styled `Green int)
          packages_stats.dune
          Fmt.(styled `Cyan int)
          packages_stats.total
          Fmt.(list ~sep:(unit ",@ ") Fmt.(styled `Red pp_package))
          packages_stats.not_dune)
  else
    Logs.app (fun l ->
        l "%aAll %a opam packages are Dune compatible! It's a spicy miracle!" Pp.Styled.header ()
          Fmt.(styled `Green int)
          packages_stats.total)
