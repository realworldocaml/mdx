# duniverse -- the spice of OCaml build life

This is an experimental vendoring system for Dune.  Not for public consumption
just yet.

Please see the manual pages for an explanation of how to use the tool.  Contact
`@avsm` if you have any queries.


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
       git tags that can be cloned into the local repository into an
       ocaml_modules subdirectory. Once the external code has been pulled
       into the repository, a single dune build command is sufficient to
       build the whole project in a standalone fashion, without opam being
       required. This is a particularly convenient way of publishing CLI
       tools to users who do not need the full power of opam.

       The basic flow of the tool is provided by three git commands:

       $ duniverse git-lock
           This converts the opam metadata into a set of git remotes, and
           stores the data in .duniverse/dune.sxp.

       $ duniverse git-pull
           Pulls the vendored code into a duniverse-of-master branch, where
           you can test the project builds.

       $ duniverse git-merge
           Merges the vendor branch into the mainline master branch.
       You can access some of the low-level functionality directly via the
       duniverse-opam, duniverse-lock and duniverse-pull commands, but this
       should not be necessary usually.

COMMANDS
       git-lock
           calculate Dune metadata and git commit the results

       git-merge
           merge vendored libraries into the mainline branch

       git-pull
           pull vendored libraries and commit them to a branch

       lock
           generate git tags suitable for vendoring from opam metadata

       opam
           analyse opam metadata to generate a standalone package list

       pull
           fetch the latest archives of the vendored libraries

       status
           summarise the libraries tracked by the duniverse

COMMON OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --version
           Show version information.

EXAMPLES
       These commands will vendor the utop and craml commands locally:

       $ duniverse git-lock --pin lwt utop craml
       $ duniverse git-pull
       $ duniverse git-merge

       Also see https://github.com/avsm/platform for an example of a fully
       bootstrapping use of this tool.

SEE ALSO
       dune(1), duniverse-git-lock(1), duniverse-git-merge(1),
       duniverse-git-pull(1), git(1), opam(1)


```

## duniverse-git-lock

```
NAME
       duniverse-git-lock - calculate Dune metadata and git commit the
       results

SYNOPSIS
       duniverse git-lock [OPTION]... [PACKAGES]...

DESCRIPTION
       This initiaises a Git repository with the vendoring metadata for Dune,
       and commits the results to the current branch. It runs 

ARGUMENTS
       PACKAGES
           opam packages to calculate duniverse for. If not supplied, any
           local opam metadata files are used as the default package list.

OPTIONS
       -b BRANCH (absent=master)
           Branch that represents the working tree of the source code. If not
           supplied, the master branch is used.

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --opam-remote=OPAM_REMOTE
           Extra opam remotes to add when resolving package names. Repeat
           this flag multiple times for more than one remote.

       -p PIN, --pin=PIN
           Packages to pin for the latest opam metadata and source. You can
           separate the package name and a url and a remote branch via commas
           to specify a manual url (e.g.
           mirage,git://github.com/avsm/mirage,fixme). If a url is not
           specified then the --dev pin is used. If a branch is not specified
           then the default remote branch is used. Repeat this flag multiple
           times for more than one exclusion.

       -r TARGET_REPO, ----repo=TARGET_REPO (absent=.)
           Path to Git repository to store vendored code in.

       -s OPAM_SWITCH, --opam-switch=OPAM_SWITCH (absent=ocaml-system)
           Name of the OCaml compiler to use to resolve opam packages. A
           local switch is created in .duniverse where pins and packages can
           be tracked independently of your main opam switch. This defaults
           to ocaml-system, but you can use this flag to supply a more
           specific version such as ocaml.4.06.1.

       --version
           Show version information.

       -x EXCLUDE, --exclude=EXCLUDE
           Packages to exclude from the output list. You can use this to
           remove the root packages so they are not duplicated in the vendor
           directory. Repeat this flag multiple times for more than one
           exclusion.

COMMON OPTIONS
       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of `auto', `always' or
           `never'.

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       -v, --verbose
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       --verbosity=LEVEL (absent=warning)
           Be more or less verbose. LEVEL must be one of `quiet', `error',
           `warning', `info' or `debug'. Takes over -v.

EXIT STATUS
       git-lock exits with the following status:

       0   on success.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SEE ALSO
       duniverse-git-merge(1), duniverse-git-pull(1)


```

## duniverse-git-pull

```
NAME
       duniverse-git-pull - pull vendored libraries and commit them to a
       branch

SYNOPSIS
       duniverse git-pull [OPTION]... 

DESCRIPTION
       This command wraps duniverse pull with a workflow that stores the
       vendored libraries in a git branch called duniverse-of-<branch>. By
       storing them in a separate branch, you can then test that they work
       (e.g. with continuous integration) and then integrate them either in
       your master branch or in a separate release branch.

       Once this command is complete, you can merge the results with
       duniverse git-merge.

OPTIONS
       -b BRANCH (absent=master)
           Branch that represents the working tree of the source code. If not
           supplied, the master branch is used.

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       -r TARGET_REPO, ----repo=TARGET_REPO (absent=.)
           Path to Git repository to store vendored code in.

       --version
           Show version information.

COMMON OPTIONS
       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of `auto', `always' or
           `never'.

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       -v, --verbose
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       --verbosity=LEVEL (absent=warning)
           Be more or less verbose. LEVEL must be one of `quiet', `error',
           `warning', `info' or `debug'. Takes over -v.

EXIT STATUS
       git-pull exits with the following status:

       0   on success.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SEE ALSO
       duniverse-git-lock(1), duniverse-git-merge(1)


```

## duniverse-status

```
NAME
       duniverse-status - summarise the libraries tracked by the duniverse

SYNOPSIS
       duniverse status [OPTION]... 

DESCRIPTION
       This command looks at the various metadata files in the .duniverse
       directory and prints them out in a human-readable format.

OPTIONS
       -b BRANCH (absent=master)
           Branch that represents the working tree of the source code. If not
           supplied, the master branch is used.

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       -r TARGET_REPO, ----repo=TARGET_REPO (absent=.)
           Path to Git repository to store vendored code in.

       --version
           Show version information.

COMMON OPTIONS
       --color=WHEN (absent=auto)
           Colorize the output. WHEN must be one of `auto', `always' or
           `never'.

       -q, --quiet
           Be quiet. Takes over -v and --verbosity.

       -v, --verbose
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       --verbosity=LEVEL (absent=warning)
           Be more or less verbose. LEVEL must be one of `quiet', `error',
           `warning', `info' or `debug'. Takes over -v.

EXIT STATUS
       status exits with the following status:

       0   on success.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).


```

