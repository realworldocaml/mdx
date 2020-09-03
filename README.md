# duniverse -- the spice of OCaml build life

This is an experimental vendoring system for Dune.  Not for public consumption
just yet.

Please see the manual pages for an explanation of how to use the tool:

- [duniverse](#duniverse): the overall usage
- [duniverse init](#duniverse-init)
- [duniverse pull](#duniverse-pull)

Contact `@avsm` if you have any queries.


# Manual Pages

## duniverse

```
NAME
       opam-monorepo - the spice of build life

SYNOPSIS
       opam-monorepo COMMAND ...

DESCRIPTION
       The opam-monorepo plugin provides a convenient interface to bridge the
       opam package manager with having a local copy of all the source code
       required to build a project using the dune build tool.

       It works by analysing opam package metadata and calculating a set of
       git tags that can be cloned into the local repository into a
       duniverse/ subdirectory. Once the external code has been pulled into
       the repository, a single dune build command is sufficient to build the
       whole project in a standalone fashion, without opam being required.
       This is a particularly convenient way of publishing CLI tools to users
       who do not need the full power of opam.

       You can access the functionality directly via the monorepo-init,
       monorepo-opam-install and monorepo-pull commands,

       Also see https://github.com/avsm/platform for an example of a fully
       bootstrapping use of this tool.

COMMANDS
       lock
           analyse opam files to generate an initial lock file

       migrate-dune-get
           convert a legacy dune-get file to an opam lock file

       opam-install
           install packages that are not duniverse-compatible via opam

       print-ocaml-compilers
           print OCaml compilers that are supported for this duniverse

       pull
           fetch the latest archives of the vendored libraries

       update
           update the commit hash corresponding to the tracked branch/tag for
           each source dependency

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --version
           Show version information.

SEE ALSO
       dune(1), git(1), opam(1)


```

