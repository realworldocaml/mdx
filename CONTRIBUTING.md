# Contributing to opam-monorepo

## Setting up your development environment

You can clone opam monorepo by running:
```
git clone git@github.com:ocamllabs/opam-monorepo.git
cd opam-monorepo
```

You should then create a local switch with just dune and ocaml installed:
```
opam switch create ./ <latest-ocaml> --no-install
opam install dune
```

opam-monorepo uses itself to lock its dependencies. These are saved in
`opam-monorepo.opam.locked`.
You will need to fetch those dependencies sources locally to be able to build
and run the test with the exact same libraries used by the CI. To do so, run:
```
opam monorepo pull
```

If you don't already have the plugin install, opam will suggest that you install
it. Once this is done it will proceed with actually running the plugin command.
`opam monorepo pull` will fetch all the sources and properly set them up in a
`duniverse/` folder.

From there you can now run the usual dune commands to build and run the tests:
```
dune build
dune runtest
```

Remember to run `opam monorepo pull` often (e.g. every time you checkout a
branch) so that your local `duniverse/` stays up to date!

## Formatting

This repository uses dune and ocamlformat to format `dune`, `.ml` and `.mli` files.

To format everything run:
```
dune build @fmt --auto-promote
```

Remember to run this command before you commit any change!
