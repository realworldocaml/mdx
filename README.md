# duniverse -- the spice of OCaml build life

This is an experimental vendoring system for Dune.  Not for public consumption
just yet.

Please see the manual pages for an explanation of how to use the tool:

- [duniverse](#duniverse): the overall usage
- [duniverse git-lock](#duniverse-git-lock): initialise vendor metadata
- [duniverse git-pull](#duniverse-git-pull): fetch latest vendor code
- [duniverse git-merge](#duniverse-git-merge): merge vendor code into master branch
- [duniverse status](#duniverse-status): see the status of the vendored code in the repo

Contact `@avsm` if you have any queries.


# Manual Pages

## duniverse

```
NAME
       duniverse - the spice of build life

SYNOPSIS
       duniverse COMMAND ...

DESCRIPTION
       The duniverse tool provides a convenient interface to bridge the opam
       package manager with having a local copy of all the source code
       required to build a project using the dune build tool.

       It works by analysing opam package metadata and calculating a set of
       git tags that can be cloned into the local repository into a
       duniverse/ subdirectory. Once the external code has been pulled into
       the repository, a single dune build command is sufficient to build the
       whole project in a standalone fashion, without opam being required.
       This is a particularly convenient way of publishing CLI tools to users
       who do not need the full power of opam.

       You can access the functionality directly via the duniverse-opam,
       duniverse-lock and duniverse-pull commands,

       Also see https://github.com/avsm/platform for an example of a fully
       bootstrapping use of this tool.

COMMANDS
       lock
           generate git tags suitable for vendoring from opam metadata

       opam
           analyse opam metadata to generate a standalone package list

       opam-install
           install packages that are not duniverse-compatible via opam

       pull
           fetch the latest archives of the vendored libraries

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

