We have a package that should not be installed via Opam. There might be various
reasons we might not want that (e.g., it cannot be built with Dune), it is
meant as a helper binary etc.

We start with a minimal `opam-repository`:

  $ gen-minimal-repo

We have a file that depends on package "b".

  $ opam show --just-file --raw -fdepends ./vendored.opam
  dune, b

This package does not set any [opam]-provided packages:

  $ opam show --just-file --raw -fx-opam-monorepo-opam-provided ./vendored.opam
  

It should lock successfully.

  $ opam-monorepo lock vendored > /dev/null

The lockfile should thus contain the package "b" and mark it as `vendor` since
`opam-monorepo` will vendor it. It should also add it to the duniverse-dirs.

  $ opam show --just-file --raw -fdepends ./vendored.opam.locked
  "b" {= "1" & ?vendor}
  "base-bigarray" {= "base"}
  "base-threads" {= "base"}
  "base-unix" {= "base"}
  "dune" {= "2.9.1"}
  "ocaml" {= "4.13.1"}
  "ocaml-base-compiler" {= "4.13.1"}
  "ocaml-config" {= "2"}
  "ocaml-options-vanilla" {= "1"}
  $ opam show --just-file --raw -fx-opam-monorepo-duniverse-dirs ./vendored.opam.locked
  [
    "https://b.com/b.tbz"
    "b"
    ["sha256=0000000000000000000000000000000000000000000000000000000000000000"]
  ]

Let's now check with the same Opam file but this one adds `opam`-provided.
Asking for the value will return that "b" will be provided by Opam:

  $ opam show --just-file --raw -fx-opam-monorepo-opam-provided ./opam-provided.opam
  b
  $ opam-monorepo lock opam-provided > /dev/null

We should see that this package is not marked as `vendor` so if we run
`opam` on it, it will install the package "b".

  $ opam show --just-file --raw -fdepends ./opam-provided.opam.locked
  "b" {= "1"}
  "base-bigarray" {= "base"}
  "base-threads" {= "base"}
  "base-unix" {= "base"}
  "dune" {= "2.9.1"}
  "ocaml" {= "4.13.1"}
  "ocaml-base-compiler" {= "4.13.1"}
  "ocaml-config" {= "2"}
  "ocaml-options-vanilla" {= "1"}

It also should not be in `duniverse-dirs` in the locked Opam file:

  $ opam show --just-file --raw -fx-opam-monorepo-duniverse-dirs ./opam-provided.opam.locked
  

Alternatively this can be done using the CLI options generated for any elements
of the solver config, including `opam-provided`:

  $ opam-monorepo lock vendored --opam-provided b > /dev/null
  $ opam show --just-file -fdepends ./vendored.opam.locked | grep '"b"'
  "b" {= "1"}

As demonstrated above, the CLI argument is taken into account and 'b' is
not vendored.

What happens in the case that a package would be ok to vendor but the
transitive dependency is `opam`-provided? In this case we have a package
`transitive` that depends on packages that will
depend transitively on an `opam`-provided package.

  $ opam show --just-file --raw -fdepends ./transitive.opam
  dune, depends-on-b
  $ opam show --just-file --raw -fx-opam-monorepo-opam-provided ./transitive.opam
  b

Locking it should work as usual

  $ opam-monorepo lock transitive > /dev/null
  $ opam show --just-file --raw -fdepends ./transitive.opam.locked
  "b" {= "1"}
  "base-bigarray" {= "base"}
  "base-threads" {= "base"}
  "base-unix" {= "base"}
  "depends-on-b" {= "1" & ?vendor}
  "dune" {= "2.9.1"}
  "ocaml" {= "4.13.1"}
  "ocaml-base-compiler" {= "4.13.1"}
  "ocaml-config" {= "2"}
  "ocaml-options-vanilla" {= "1"}

`depends-on-b` is vendored (since it can) but "b" is `opam`-provided. Neat.

Now for the reverse case, `depends-on-b` is `opam`-provided.

  $ opam show --just-file --raw -fdepends ./reverse-transitive.opam
  dune, depends-on-b
  $ opam show --just-file --raw -fx-opam-monorepo-opam-provided ./reverse-transitive.opam
  depends-on-b

Since it is, we need to make its dependency, "b", also `opam`-provided.

  $ opam-monorepo lock reverse-transitive > /dev/null
  $ opam show --just-file --raw -fdepends ./reverse-transitive.opam.locked
  "b" {= "1"}
  "base-bigarray" {= "base"}
  "base-threads" {= "base"}
  "base-unix" {= "base"}
  "depends-on-b" {= "1"}
  "dune" {= "2.9.1"}
  "ocaml" {= "4.13.1"}
  "ocaml-base-compiler" {= "4.13.1"}
  "ocaml-config" {= "2"}
  "ocaml-options-vanilla" {= "1"}

It should also work to pass the version of the compiler and be respected for
both `opam`-provided packages as well as those to be vendored:

  $ opam-monorepo lock opam-provided --ocaml-version=4.13.1 > /dev/null
