[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focamllabs%2Fopam-monorepo%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/ocamllabs/opam-monorepo)

# opam-monorepo

`opam-monorepo` is an opam plugin designed to assemble standalone dune workspaces
with your projects and all of their dependencies, letting you build it all from scratch
using only `dune` and `ocaml`.

## Installation

You can simply install it via opam in your current switch by running:

```
opam install opam-monorepo
```

Note that once it is installed you can invoke it as an opam command:
```
opam monorepo <subcommand> ...
```

Note that opam knows about available plugins and will offer you to install opam-monorepo
if you try to invoke it without having installed it beforehand.

## Usage

The basic usage for opam-monorepo is to start by running the following command from the root
of your project:
```
opam monorepo lock
```

This will generate a project-wide lockfile under `<project-name>.opam.locked` that contains:
- The full list of your direct and transitive opam dependencies, according to the specifications
  in the `.opam` files at your project's root, with hard version constraints (`{= <version>`) in
  the `depends` field.
- A `pin-depends` field filled with reproducible pins, either to tarballs or to git repos with an
  explicit commit hash for all the above dependencies, except for `ocaml`, `dune` and any virtual
  opam package.
- A few extra fields meant to be interprated by `opam monorepo`

This lockfile can then be consumed by the following command:
```
opam monorepo pull
```
which will fetch the sources of all the locked dependencies into a `duniverse/` folder at the root of
your project, marking them as `vendored_dirs` (see
[dune's documentation](https://dune.readthedocs.io/en/latest/dune-files.html#vendored-dirs-since-1-11))
so that dune will only build the artifacts you need from that folder.

From that point you should be able to run `dune build` and `dune runtest` as you normally would and
build your entire project from scratch!

### opam monorepo lock

It's important to note that `opam monorepo lock` will only succeed if all of your non-virtual and
non "base" dependencies (e.g. `ocaml` or `dune`) build with dune (i.e. directly depend on the `dune`
or `jbuilder` packages).
If that's not the case the solver will report which packages don't build with dune.

We maintain a [separate opam repository](https://github.com/dune-universe/opam-overlays) with dune
ports of commonly used opam packages. If you have non-dune dependencies it is advised that you add
this repository before running `opam monorepo lock`. You can do so by running the following command:
```
opam repository add dune-universe git+https://github.com/dune-universe/opam-overlays.git
```
Note that if it is not setup, the plugin will warn you.

The `lock` command takes your global and switch's opam configurations into account, meaning any
opam repository or pins you set up will be picked up by the solver when resolving the full set of
your project's dependencies.

The generated lockfile is meant to be compatible with `opam` in such a way that running `opam
install . --locked` should give you the same versions you would using `opam monorepo pull` in
a reproducible way (i.e. independently of any change that might have happened on the upstream
opam-repository) thanks to the `pin-depends`.
You can use that property to your advantage by allowing one to choose between a "monorepo" or
regular opam workflow depending on the situation.

### opam monorepo pull

The `pull` command fetches the sources using the URLs in the lockfile. It benefits from the opam
cache but its outcome does not depend on your opam configuration.

## Monorepo projects

If you wish to use `opam-monorepo` to manage your dependencies, we suggest that you git version the
lockfile but not the content of the `duniverse/`.

If you use [ocaml-ci](https://github.com/ocurrent/ocaml-ci) and have an opam-monorepo lockfile at
the root of your project, it will detect it is an opam-monorepo project and start a specific
pipeline in which it will use the plugin to assemble a dune workspace with your dependencies rather
than installing them through opam.
