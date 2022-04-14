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

open Duniverse_cli

let cmds = [ Lock.cmd; Pull.cmd; Depext.cmd; List_cmd.cmd ]

let init_opam () =
  OpamSystem.init ();
  let root = OpamStateConfig.opamroot () in
  ignore (OpamStateConfig.load_defaults root);
  OpamFormatConfig.init ();
  OpamCoreConfig.init ~safe_mode:true ();
  OpamRepositoryConfig.init ();
  OpamStateConfig.init ~root_dir:root ()

let default_run () = `Help (`Pager, None)
let default = Cmdliner.Term.(ret (const default_run $ const ()))

let info =
  let open Cmdliner in
  let doc = "the spice of build life" in
  let sdocs = Manpage.s_common_options in
  let man_xrefs = [ `Tool "dune"; `Tool "opam"; `Tool "git" ] in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The $(tname) plugin provides a convenient interface to bridge the \
         $(b,opam) package manager with having a local copy of all the source \
         code required to build a project using the $(b,dune) build tool.";
      `P
        "It works by analysing opam package metadata and calculating a set of \
         URLs that can be downloaded or cloned into the local repository into \
         a $(b,duniverse/) subdirectory. Once the external code has been \
         pulled into the repository, a single $(b,dune build) command is \
         sufficient to build the whole project in a standalone fashion, \
         without opam being required. This is a particularly convenient way of \
         publishing CLI tools to users who do not need the full power of opam.";
      `P
        "You can access the functionality directly via the $(i,monorepo-lock) \
         and $(i,monorepo-pull) commands,";
      `P
        "Also see $(i,https://github.com/avsm/platform) for an example of a \
         fully bootstrapping use of this tool.";
    ]
  in
  Cmd.info "opam-monorepo" ~version:Common.Arg.version ~doc ~man_xrefs ~sdocs
    ~man

let main = Cmdliner.Cmd.group ~default info cmds

let () =
  init_opam ();
  Stdlib.exit @@ Cmdliner.Cmd.eval' main
