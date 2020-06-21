(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

let get_opam_publish_file p opam_publish_file = match opam_publish_file with
| Some file -> Ok file
| None ->
    Topkg_care.Pkg.build_dir p
    >>= fun bdir -> Topkg_care.Pkg.distrib_filename ~opam:true p
    >>= fun fname -> Ok Fpath.(bdir // fname + ".opam")

let descr pkg =
  Topkg_care.Pkg.opam_descr pkg >>= fun (descr, _) ->
  Logs.app (fun m -> m "%s" (Topkg_care.Opam.Descr.to_string descr));
  Ok 0

let pkg pkg dist_pkg opam_publish_file =
  let log_pkg dst =
    Logs.app (fun m -> m "Wrote opam package %a" Topkg_care.Pp.path dst)
  in
  let warn_if_vcs_dirty () =
    Cli.warn_if_vcs_dirty "The opam package may be inconsistent with the \
    distribution."
  in
  let opam_file_content opam (descr, from_opam) url =
    let descr =
      if from_opam then "" else
      (Topkg_care.Opam.Descr.to_opam_fields descr ^ "\n")
    in
    strf "%s\n%s%s" opam descr (Topkg_care.Opam.Url.to_opam_section url)
  in
  get_opam_publish_file pkg opam_publish_file
  >>= fun dst -> Topkg_care.Pkg.opam pkg
  >>= fun opam -> OS.File.read opam
  >>= fun opam -> Topkg_care.Pkg.opam_descr pkg
  >>= fun descr -> Topkg_care.Pkg.distrib_file dist_pkg
  >>= fun distrib_file -> Topkg_care.Pkg.distrib_uri dist_pkg
  >>= fun uri -> Topkg_care.Opam.Url.with_distrib_file ~uri distrib_file
  >>= fun url -> OS.File.write dst (opam_file_content opam descr url)
  >>= fun () -> log_pkg dst; warn_if_vcs_dirty ()
  >>= fun () ->
  Ok 0

let submit pkg opam_pkg_dst =
  Topkg_care.Opam.ensure_publish ()
  >>= fun () -> get_opam_publish_file pkg opam_pkg_dst
  >>= fun opam_file -> OS.File.exists opam_file
  >>= function
  | false ->
      Logs.err (fun m -> m "Package@ file %a@ does@ not@ exist. Did@ you@ \
                            forget@ to@ invoke 'topkg opam pkg' ?"
                   Fpath.pp opam_file);
      Ok 1
  | true ->
      Logs.app (fun m -> m "Publishing %a" Topkg_care.Pp.path opam_file);
      Topkg_care.Pkg.publish_msg pkg
      >>= fun msg -> Topkg_care.Opam.submit ~opam_file ~msg >>= fun () -> Ok 0

let field pkg field = match field with
| None -> Logs.err (fun m -> m "Missing FIELD positional argument"); Ok 1
| Some field ->
    Topkg_care.Pkg.opam_field pkg field
    >>= function
    | Some v -> Logs.app (fun m -> m "%s" (String.concat ~sep:" " v)); Ok 0
    | None ->
        Topkg_care.Pkg.opam pkg >>= fun opam ->
        Logs.err (fun m -> m "%a: field %s is undefined" Fpath.pp opam field);
        Ok 1

(* Command *)

let opam () pkg_file build_dir
    dist_name dist_version dist_opam dist_uri dist_file
    opam_publish_file pkg_name pkg_version pkg_opam pkg_descr
    readme change_log publish_msg action field_name
  =
  let p =
    Topkg_care.Pkg.v
      ?build_dir ?name:pkg_name ?version:pkg_version ?opam:pkg_opam
      ?opam_descr:pkg_descr ?readme ?change_log ?publish_msg pkg_file
  in
  begin match action with
  | `Descr -> descr p
  | `Pkg ->
      let dist_p =
        Topkg_care.Pkg.v
          ?build_dir ?name:dist_name ?version:dist_version ?opam:dist_opam
          ?distrib_uri:dist_uri ?distrib_file:dist_file ?readme ?change_log
          ?publish_msg pkg_file
      in
      pkg p dist_p opam_publish_file
  | `Submit -> submit p opam_publish_file
  | `Field -> field p field_name
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  let action = [ "descr", `Descr; "pkg", `Pkg; "submit", `Submit;
                 "publish", `Submit; "field", `Field ]
  in
  let doc = strf "The action to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum action)
  in
  let action = Arg.enum action in
  Arg.(required & pos 0 (some action) None & info [] ~doc ~docv:"ACTION")

let field =
  let doc = "the field to output ($(b,field) action)" in
  Arg.(value & pos 1 (some string) None & info [] ~doc ~docv:"FIELD")

let opam_publish_file =
  let doc = "The file to use to publish the opam package. If absent the
             file $(i,BUILD_DIR)/$(i,PKG_NAME).$(i,PKG_VERSION).opam in the
             build directory is used (see options $(b,--build-dir),
             $(b,--pkg-name) and $(b,--pkg-version))"
  in
  let docv = "FILE" in
  Arg.(value & opt (some Cli.path_arg) None & info ["opam-publish-file"]
         ~doc ~docv)

let pkg_version =
  let doc = "The version string $(docv) of the opam package. If absent provided
             provided by the VCS tag description of the HEAD commit."
  in
  let docv = "PKG_NAME" in
  Arg.(value & opt (some string) None & info ["pkg-version"] ~doc ~docv)

let pkg_opam =
  let doc = "The opam file to use for the opam package. If absent uses the
             opam file mentioned in the package description that corresponds
             to the opam package name $(i,PKG_NAME) (see option
             $(b,--pkg-name))"
  in
  let docv = "FILE" in
  Arg.(value & opt (some Cli.path_arg) None & info ["pkg-opam"] ~doc ~docv)

let pkg_descr =
  let doc = "The opam descr file to use for the opam package. If absent
             and the opam file has synopsis and description fields this
             is used for the description. It absent and there are no such
             fields in the opam file and the opam file name
             (see $(b,--pkg-opam)) has a `.opam`
             extension, uses an existing file with the same path but a `.descr`
             extension. If the opam file name is `opam` uses a `descr`
             file in the same directory. If these files are not found
             a description is extracted from the the readme (see
             option $(b,--readme)) as follow: the first marked up
             section of the readme is extracted, its title is parsed
             according to the pattern '\\$(NAME) \\$(SEP) \\$(SYNOPSIS)',
             the body of the section is the long description. A few
             lines are filtered out: lines that start with either
             'Home page:', 'Contact:' or '%%VERSION'."
  in
  let docv = "FILE" in
  Arg.(value & opt (some Cli.path_arg) None & info ["pkg-descr"] ~doc ~docv)

let doc = "Interaction with opam and the OCaml opam repository"
let sdocs = Manpage.s_common_options
let envs =
  [ Term.env_info "TOPKG_OPAM_PUBLISH" ~doc:"The $(b,opam-publish) tool to use
    to submit packages." ]

let man_xrefs = [`Main; `Cmd "distrib" ]
let man =
  [ `S Manpage.s_synopsis;
    `P "$(mname) $(tname) [$(i,OPTION)]... $(i,ACTION)";
    `S Manpage.s_description;
    `P "The $(tname) command provides a few actions to interact with
        opam and the OCaml opam repository.";
    `S "ACTIONS";
    `I ("$(b,descr)",
        "extract and print an opam descr file. This is used by the
         $(b,pkg) action. See the $(b,--pkg-descr) option for details.");
    `I ("$(b,pkg)",
        "create an opam package description for a distribution.
         The action needs a distribution archive to operate, see
         topkg-distrib(1) or the $(b,--dist-file) option.");
    `I ("$(b,submit) or $(b,publish)",
        "submits a package created with the action $(b,pkg) the OCaml
         opam repository. Requires the $(b,opam-publish) tool to be
         installed.");
    `I ("$(b,field) $(i,FIELD)",
        "outputs the field $(i,FIELD) of the package's opam file."); ]

let cmd =
  let info = Term.info "opam" ~doc ~sdocs ~envs ~man ~man_xrefs in
  let t = Term.(pure opam $ Cli.setup $ Cli.pkg_file $ Cli.build_dir $
                Cli.dist_name $ Cli.dist_version $ Cli.dist_opam $
                Cli.dist_uri $ Cli.dist_file $
                opam_publish_file $ Cli.pkg_name $ pkg_version $ pkg_opam $
                pkg_descr $ Cli.readme $ Cli.change_log $ Cli.publish_msg $
                action $ field)
  in
  (t, info)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
