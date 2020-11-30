* [API docs][]

# Introduction

Opam's default solver is designed to maintain a set of packages over time,
minimising disruption when installing new programs and finding a compromise
solution across all packages (e.g. avoiding upgrading some library to prevent
uninstalling another program).

In many situations (e.g. a CI system building in a clean environment, a
project-local opam root, or a duniverse build) this is not necessary, and we
can get a solution much faster by using a different algorithm.

This package does that by using 0install's (pure OCaml) solver with opam
packages.

# Usage

Run the `opam-0install` binary with the packages you want to install:

```bash
$ dune exec -- opam-0install utop
[NOTE] Opam library initialised in 0.16 s
base-bigarray.base base-bytes.base base-threads.base base-unix.base camomile.1.0.2 charInfo_width.1.1.0 conf-m4.1 cppo.1.6.6 dune.2.1.3 dune-configurator.2.1.3 dune-private-libs.2.1.3 lambda-term.2.0.3 lwt.5.1.1 lwt_log.1.1.1 lwt_react.1.1.3 mmap.1.1.0 ocaml.4.09.0 ocaml-base-compiler.4.09.0 ocaml-config.1 ocamlbuild.0.14.0 ocamlfind.1.8.1 ocplib-endian.1.0 react.1.2.1 result.1.4 seq.base topkg.1.0.1 utop.2.4.3 zed.2.0.4
[NOTE] Solve took 0.25 s
```

Note: the first run may be slow, as the opam library it uses may decide to rebuild its index first.

`opam-0install` outputs the set of packages that should be installed (but doesn't install them itself).
The output is in a format suitable for use as input to `opam`. e.g.

```bash
opam install $(opam-0install utop)
```

Note that it does not look at the current switch's OCaml version and may therefore choose a newer (or older) one.
You can pass the version explicitly to constrain it. e.g.

```bash
opam-0install utop ocaml.4.08.1
```

or

```bash
opam-0install utop 'ocaml<4.09'
```

You can also pass other packages and constraints here too, as with opam itself.
`opam-0install` will optimise the packages in order, so `opam-0install foo bar` will always pick the
newest possible version of `foo`, even if that means choosing an older version of `bar`
(but it will choose an older version of `foo` if there is no other way to get `bar` at all).

# Tests

Running `make test` will run various tests (some fixed and some random) using
both opam-0install and opam's solver and compare the results.

When testing changes to the code, you may want to do:

    dune exec -- ./test/dump.exe --jobs=NN baseline.csv
    [ make changes ]
    dune exec -- ./test/dump.exe --jobs=NN new.csv

`dump.exe` takes each package name in opam-repository and solves for it
individually, generating a CSV file with the solutions. `NN` is the number of
processes to use to speed it up; use the number of cores your machine has.
Depending on the speed of your computer, this is likely to take several
minutes.

You may want to use `--root` to use a separate opam root directory, e.g.

    opam init --no-setup --root ./opam-root /path/to/opam-repository

This means you can upgrade your default opam root without changing the test results.

To compare the results, use:

    dune exec -- ./test/diff.exe baseline.csv new.csv

# API

The library provides these sub-modules under `Opam_0install`:

- `Solver` is used to create a solver, given an `S.CONTEXT` (source of opam packages).
- `Switch_context` gets packages from the user's opam switch, including any pinned packages.
- `Dir_context` reads the packages directly from a checkout of `opam-repository`.
- `Model` is used internally by `Solver`. It provides the interface needed by 0install.

Example:

```ocaml
let env =
  Opam_0install.Dir_context.std_env
    ~arch:"x86_64"
    ~os:"linux"
    ~os_family:"debian"
    ~os_distribution:"debian"
    ~os_version:"10"
    ()

let context =
  Opam_0install.Dir_context.create "/tmp/opam-repository/packages"
    ~constraints:OpamPackage.Name.Map.empty
    ~env

module Solver = Opam_0install.Solver.Make(Opam_0install.Dir_context)

let () =
  let result = Solver.solve context [OpamPackage.Name.of_string "utop"] in
  match result with
  | Error e -> print_endline (Solver.diagnostics e)
  | Ok selections ->
    Solver.packages_of_result selections
    |> List.iter (fun pkg -> Printf.printf "- %s\n" (OpamPackage.to_string pkg))
```

# Internals

The core 0install solver does not depend on the rest of 0install and just
provides a functor that can be instantiated with whatever package system you
like (see [Simplifying the Solver With Functors][]). [Zeroinstall_solver.S][]
describes the interface required by the `0install-solver` package.

`opam-0install` provides an implementation of this interface using opam package
metadata. It's a little complicated because 0install doesn't support
alternatives in dependencies (e.g. `ocaml-config` depends on
`"ocaml-base-compiler" | "ocaml-variants" | "ocaml-system"`). The mapping
introduces a "virtual" package in these cases (so `ocaml-config` depends on a
virtual package that has three available versions, with dependencies on the
real packages).

A virtual package is also created if you specify multiple packages on the command-line.

[Zeroinstall_solver.S]: https://0install.github.io/0install/0install-solver/Zeroinstall_solver/S/index.html
[Simplifying the Solver With Functors]: https://roscidus.com/blog/blog/2014/09/17/simplifying-the-solver-with-functors/
[API docs]: https://ocaml-opam.github.io/opam-0install-solver/
