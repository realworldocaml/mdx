let run () = `Help (`Pager, None)

let info =
  let open Cmdliner in
  let doc = "the spice of build life" in
  let sdocs = Manpage.s_common_options in
  let man_xrefs = [ `Tool "dune"; `Tool "opam"; `Tool "git" ] in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The $(tname) tool provides a convenient interface to bridge the $(b,opam) package manager \
         with having a local copy of all the source code required to build a project using the \
         $(b,dune) build tool.";
      `P
        "It works by analysing opam package metadata and calculating a set of git tags that can be \
         cloned into the local repository into a $(b,duniverse/) subdirectory. Once the external \
         code has been pulled into the repository, a single $(b,dune build) command is sufficient \
         to build the whole project in a standalone fashion, without opam being required. This is \
         a particularly convenient way of publishing CLI tools to users who do not need the full \
         power of opam.";
      `P
        "You can access the functionality directly via the $(i,duniverse-init), \
         $(i,duniverse-opam-install) and $(i,duniverse-pull) commands,";
      `P
        "Also see $(i,https://github.com/avsm/platform) for an example of a fully bootstrapping \
         use of this tool.";
    ]
  in
  Term.info "duniverse" ~version:"%%VERSION%%" ~doc ~man_xrefs ~sdocs ~man

let term = Cmdliner.Term.(ret (const run $ pure ()))

let cmd = (term, info)
