This document the `duniverse` tool aims and next steps.

Overview
--------

The role of the duniverse tool is to setup a development environment for
the dune build system, or more precisely a "workspace". Its
distinguishing characteristic is that it can setup workspaces composed
of multiple projects, bringing hassle free multi-project development
to everyone.

To operate, the duniverse tool relies on an external package
manager. In version 1.0, the only supported package manager is
opam.  The user only needs to have opam installed but not setup,
since this tool takes care of initialising it.

Finally, the duniverse tool is tightly integrated with git. In
particular, the workspaces it creates and manages will always be
versioned controled by git. This ensures that users can access the
history of the workspace as well as of individual projects inside it.

Specification
-------------

The duniverse tool should provide various commands for initializing,
updating and inspecting the status a dune workspace. In the version
1.0 of the tool, it is only required to support initializing a
workspace and inspecting its status.

We are now describing these two commands.

### duniverse init

This command initialises a dune workspace. When doing so, the user
provides a set of "main" packages that they want to work on. The
duniverse will then provides a working dune workspace where the user
can work on all these packages at once.

To understand better what it means, let's consider the following
examples:

#### Example 1: working on lwt only

If the user wants to work on lwt, they need the following:

- the source code of the lwt project
- all the dependencies of lwt pre-compiled
- a few development tools such as merlin, ocamlformat, ...

When initialising a workspace to work on lwt, the duniverse tool will
do the following:

1. make sure all the dependencies of lwt are installed via opam
2. make sure all the necessary development tools are installed via opam
3. clone the source code repository of lwt

At this point, the developer will have everything they need to start
working on lwt, making patches and submitting them.

#### Example 2: working on both zed and utop

A user wants to add a new feature to the utop toplevel. However doing
so requires adding new functions to the zed library. To initialize the
workspace, the duniverse tool will have to do the same as before and
close both the zed and utop repositories in step 3.

However, there is one additional point to consider: utop depends on
the lambda-term library and the lambda-term library depends on zed. If
we rely on the lambda-term library installed via opam, then we will
have two conflicting version of zed when linking the utop binary: the
one that is a dependency of lambda-term in opam and the one that is in
the workspace. This is not allowed.

To solve this problem, the duniverse tool will additionally need to
clone the source code repository of lambda-term and add it to the
workspace. Because lambda-term is not a package the user wants to work
on, the duniverse tool should put it in a separate directory so that
the developer's attention stays focus on the packages they want to
work on. In the rest of this document, we will call such packages
"intermediate".

#### Specifying the main packages

When calling `duniverse init`, the user has two way of specifying the
main packages:

1. via the file system
2. via the command line

To specify the package via the file system, the user should simply
have already cloned the source code repository of the main
packages. The duniverse tool will assume that any for `<package>.opam`
files it finds, `<package>` is a main package the user wants to work
on. duniverse will look for these files in the current directory and
in sub-directories in the same way dune crawls the file system. To
help with this task, dune will provide a `dune list-opam-files`
command to report the list of `<package>.opam` files to consider.

To specify the package via the file system, the user should simply
list them as argument of `duniverse init`. `duniverse init` accepts
several ways of specifying a package:

- as an opam package name, for instance `lwt`. In this case, the
  source code repository declared in the `dev-repo` field of the
  corresponding opam package will be used
- as a repository url, for instance `https://github.com/ocsigen/lwt`

For convenience, arguments such as `ocsigen/lwt` will be interpreted
as `https://github.com/ocsigen/lwt`.

So for instance, all the following scenarios are equivalent:

```
$ git clone https://github.com/ocsigen/lwt
$ duniverse init

-----

$ duniverse init https://github.com/ocsigen/lwt

-----

$ duniverse init ocsigen/lwt
```

#### Versions consideration

In the previous section, we assumed that the user wanted to work with
the master branch of all the repository involved. However, the
duniverse tool also allows to work with alternative "release"
branches. Duniverse will support the following scheme:

- the `master` branch is for released opam packages
- the `dev` branch is for bleeding edge development

When invoking `duniverse init`, the user may specify what branch of
the package they want to work on by adding a `--version <version>`
argument after each package name. When no such argument are specified,
the duniverse tool will assume that the user wants to work on the
latest stable version of opam packages.

#### Pinning the dev version of a dependency

TODO how to pin just one package? Mark it in `dune-get` file perhaps.

### Checking the status of a workspace

The `duniverse status` will report the state of a workspace. In
particular, it will report the list of main and intermediate packages
in the workspace as well as their version, as reported by `git
describe --dirty --always`.

Implementation
--------------

Duniverse will use the opam libraries in order to determine the list
of intermediate package it should import in the workspace and external
packages it must install.

Duniverse requires that all main and intermediate packages are using
dune. Since not all OCaml packages are currently using dune, it will
consider overlays hosted on https://github.com/dune-universe. Such overlays
are forks of the upstream projects where the `dune` files have been
added in order to build the project with dune.

## Phase 1

The current duniverse repository has a lot of metadata that it stores.
Specifically, it has a `.duniverse` directory in which it has:

- `opam.sxp`: stores the conversion from the root packages specified
  and invokes the opam solver to get a complete list of packages.

- `dune.sxp`: converts the list of opam packages into a concrete set
  of git references. It uses heuristics for this since we only have
  the archive file in a release, not the git tag.

- `_opam`: is an opam2 local switch.

We can get rid of all this temporary metadata by:

- Adding a `dune list-opam-files` command to dune which tells us what the
  active local packages are.

- Reinitialise the opam state on every invocation of `init` so we dont
  have to bother with `opam update` of a local switch.  We could also
  then just store the `OPAMROOT` in a tmp dir so that it is not in the
  same directory as the source code repository. (Lucas had a problem
  with `opam pin` of a duniverse repository since opam then rsynched
  the entire opam2 local switch).

- Store the list of git remotes in a `dune-get` file in the local
  repository.  This can eventually be merged into dune-project in a
  future revision of dune.

The `config.ml` in duniverse also currently hardcodes the list of
packages that have overrides.  Instead, we should turn the
dune-universe/ github organisation into a proper opam remote, where
the dune variants of packages are stored in a versioned fashion.

For example, a dune-universe remote for zarith would be:

- `zarith.1.7+dune`: have this package in dune-universe/opam-repository
- `dune-universe/zarith`: has a branch for the zarith-1.7 port (currently
  this is the `duniverse-1.7` branch for example).

With an opam remote, there is no more need for the duniverse tool to know about
dune overrides.  Instead, it just adds `dune-universe/opam-repository` to its
remote list before calculating the packages, and that is sufficient to pull in
the overridden packages.

TODO: what should the name of the vendor directory be? We currently use
`ocaml_modules` since `vendor/` is used by some packages (such as ocamlformat).

The automatic git invocations have also been problematic.  For phase 1,
we could remove all automatic `git commit` and simply fetch the archives
for the user to manually git add/remove/commit.

## Phase 2

- Integrate the `dune-get` file directly into `dune-project`.
- Support making changes directly into the vendored packages and opening PRs.

--
- contacts: @dim @avsm @samoht
