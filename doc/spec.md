# Duniverse

## High level spec

The goal of `duniverse` is to help you setup a workspace in which you only rely on dune to build
your entire project, including its transitive dependencies.

It should also make it easy for you to work on your dependencies, modify them, open PRs upstream
or push your changes to forks to be able to reuse them while making sure none of this breaks your
project or any of your other dependencies.

`duniverse` relies on a `duniverse.sxp` file, at the root of the project to operate. It contains
all the information needed to gather and inspect all of your transitive dependencies.
For each package it know its name, the upstream git repository URL, the specific git reference
for the version you're using as well as the eventual fork repository URL you're using instead of
the upstream one.
The `duniverse.sxp` file should be part of your project repository as it specifies how to locally
pull your dependencies to be able to build your project.
It is in a human readable sexp format and you should be able to edit it by hand in some cases where
the `duniverse` tool doesn't cover your specific use case.

`duniverse` will pull your dependencies in a `duniverse/` directory at the root of your project.
It will clone the sources using `git` and check them out to the right tag, branch or commit. 
The `duniverse/` directory contains a subdir for each of your transitive dependencies, named after
the opam package they correspond to or using the name you provided when adding a non-opam
dependency.
This directory will be setup so that running `dune build` or `dune runtest` from the root of your
project will only build your public targets and run your tests but not the ones in `duniverse/`
unless you explictly depend on them.
Each subdir in `duniverse/` is a regular git project, meaning that you can easily `cd` into one
and start working on it with your regular git workflow. The `duniverse` tool provides helpers to
manage those changes and keep track of them in the `duniverse.sxp` file.

Because `duniverse` only supports packages that are built with `dune`, it will complain if a
dependency cannot be resolved to a version that is built with `dune`. Hopefully, to work around
that it will also look into the [`dune-universe/opam-overlays`]() opam repository for dunified
forks of package which haven't been ported upstream yet. When a dependency from the `opam-overlays`
repo is picked, it will consider it's git URL as the upstream one and not the actual upstream
package repo URL.

### System constraints

`duniverse` requires that your system has OCaml and `dune` installed, in the right versions.

It uses a vendored `opam` for dependencies resolution so you don't need a local `opam`.

### Duniverse commands

#### `duniverse init`

`duniverse init` will generate an initial `duniverse.sxp` file.
It will look for `.opam` files recursively and try to resolves the joint dependencies of all such
packages it finds. If the dependencies can't be resolved because of conflicts, it will fail.
It will warn you about the dependencies for which it couldn't find a dunified version and write the
other ones to the `duniverse.sxp`.
It should be run from the root of your project as it will write the `duniverse.sxp` in the current
working dir and start looking for `.opam` files from there as well.

#### `duniverse pull`

`duniverse pull` will pull your dependencies in the `duniverse/` directory at the root of your
project where the `duniverse.sxp` file is located. It will install them according to what's declared
in the `duniverse.sxp` file.

If the `duniverse/` directory is dirty ie if it contains some local changes and isn't aligned with
the `duniverse.sxp` file, `duniverse pull` will warn you about it and abort unless explicitly told
to overwrite those changes via a command line flag.

#### `duniverse status`

`duniverse status` tells you which of your dependencies are in a "dirty" state ie which of the
sub directories in `duniverse/` contain uncommitted changes or are checked out to a different ref
than the one declared in the `duniverse.sxp`

`duniverse status <package_or_sub_dir>` gives you detailed information about how the given package
differs from the sources of the declared dependency.

#### `duniverse fork`

`duniverse fork` assists you in committing local changes to a dependency, pushing them to a fork and
updating your `duniverse.sxp` in one go.

#### `duniverse install`

`duniverse install` allows you to try and install a new dependency. Assuming it can be resolved
without conflicting with your currently installed ones, the `duniverse.sxp` file is updated and its
sources are cloned into `duniverse/`.

`duniverse install` will use the opam files from the `duniverse/` folder and the one from the
package you're trying to install to ensure there is no conflicts.

You can use `duniverse install` to add a new opam package to your dependencies without going through
the entire pipeline again but it can also be used to install custom dependencies such as the ones
that `duniverse init` couldn't resolve but you know where to find.

Ultimately you can provide `duniverse install` every detail about what you're installing such as the
name you want for this package, its upstream URL, the git reference and eventually the fork url so
it updates the `duniverse.sxp` and the `duniverse/` dir in one go for you.

## User stories

### André, builds a CLI tool

André works on a CLI tool. He wants to keep track of what it is exactly he needs to vendor and will
in the end be compiled into its native binary. Because he doesn't care about making it work with a
range of OCaml or libraries versions he's happy to pin each of its dependencies to a specific version
to keep the builds reproducible and make it easier to work with his team. He has had issues with
opam in the past trying to reproduce old builds because the opam repository had changed in the
meantime.

To achieve all of this while keeping things simple he decides to use `duniverse`.

He installs `duniverse` on his system. He then runs `duniverse init` from the root of his project.
Unfortunately for André, one of its transitive dependencies, `some-important-library`, isn't build
with `dune`. He is willing to use `duniverse` so he decides to port version `0.8.0`, the one he
needs, to dune.

To do so he creates a fork of the upstream repo on Github, checks out to the `v0.8.0` tag and starts
working on porting it to dune. He manages to get the whole thing to build, run the tests and
install. He commits his changes to a `dune-universe-v0.8.0` branch and tags it `v0.8.0+dune`.
He then opens a PR to the `dune-universe/opam-overlays` repo adding the
`packages/some-important-library/some-important-library.0.8.0+dune/opam` file with the `url.src`
field properly set to `"git+https://github.com/andre/some-important-library#v0.8.0+dune"`.
Because the `dune-universe` team is so committed, the PR is reviewed and merged within the hour.

He runs `duniverse init` once again and gets a message telling him everything went smoothly, all the
dependencies could be resolved and the `duniverse.sxp` file was correctly written. Because André
doesn't blindly trust any CLI tool, he runs `duniverse install` to fetch all the dependencies and
install them in the `duniverse/` folder, move to a fresh and empty opam switch and runs
`dune build @default @runtest` to make sure his tool builds correctly and the test suite is properly
executed. Because the `duniverse` team did an amzing job, everything works as intended.
He then removes the `.opam` file and commits the new `duniverse.sxp`. André really cares about build
reproducibility and he wants a fast deterministic CI with no race conditions and as few downloads as
possible, he decides to also commit the content of the `duniverse/` folder. That ensures from that
point forward, he will always be able to reproduce any build, even if the upstream branches, tags or
even repos get removed. That also means that his CI doesn't rely on internet access to install
dependencies which will prevent the occasional timeout. Because he doesn't want to encourage local
changes to dependencies, he makes sure the CI runs `duniverse status --exit-code`, which fails if
the `duniverse.sxp` and `duniverse/` aren't in sync.

### Bérénice, works on Mirage libraries

### Charlotte, wants to build unikernels

## Implementation consideration

### The `duniverse.sxp` file

They should be human readable, at least as human readable as `dune` files.

They should be part of the git history of your project.

They should contain information allowing to easily implement all of the above, a package stanza should
look like this:
```
(duniverse_package
 (name <duniverse_sub_dir_name>)
 (opam_packages
    <package_1>
    <package_2>)
 (upstream
  (remote_name origin)
  (url "https://github.com/their_org/name")
  (git_ref v0.5.2))
 (fork
  (remote_name my-fork)
  (url "https://github.com/my_org/name")
  (git_ref <some_commit_ref>))
```

The `opam_packages` field should be present to help with handling multi-opam repos.

The two separate `upstream` and `fork` fields allows `duniverse` to help you keep track
of the changes you made to one of your dependency.
