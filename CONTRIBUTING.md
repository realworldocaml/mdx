# Contributing to `opam-monorepo`

## Setting Up Your Development Environment

You can clone `opam-monorepo` by running:
```
git clone git@github.com:ocamllabs/opam-monorepo.git
cd opam-monorepo
```

`opam-monorepo` uses itself to lock its dependencies. These are saved in
`opam-monorepo.opam.locked`.

You will need to setup a local switch and install Dune and OCaml as specified
by the lock file:
```
opam switch create --empty ./
opam install --deps-only --ignore-pin-depends ./opam-monorepo.opam.locked
```

You will need to fetch the of the dependencies sources locally to be able to
build and run the test with the exact same libraries used by the CI. To do so,
run:
```
opam monorepo pull
```

If you don't already have the plugin installed, opam will suggest that you install
it. Once this is done it will proceed with actually running the plugin command.
`opam monorepo pull` will fetch all the sources and properly set them up in a
`duniverse/` folder.

Note that this repository's lock file should be handled using the version of
`opam-monorepo` corresponding to the lockfile. This will always correspond
to a released version. We do not use the dev version of opam-monorepo to prevent
bootstrapping issues.

From there you can now run the usual Dune commands to build and run the tests:
```
dune build
dune runtest
```

Remember to run the above mentioned steps often (e.g., every time you checkout a
branch) so that your local `duniverse/` and switch stay up to date!

## Formatting

This repository uses Dune and OCamlformat to format `dune`, `.ml` and `.mli` files.

To format everything run:
```
dune build @fmt --auto-promote
```

Remember to run this command before you commit any change!
