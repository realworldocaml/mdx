We have a package that should not be installed via opam. There might be various
reasons we might not want that (e.g. it cannot be built with `dune`), it is
meant as a helper binary etc.

We start with a minimal opam-repository:

  $ gen-minimal-repo

We have a file that depends on package `b`.

  $ opam show --no-lint --raw -fdepends ./vendored.opam
  "dune" "b"

This package does not set any `opam-provided` packages:

  $ opam show --no-lint --raw -fx-opam-monorepo-opam-provided ./vendored.opam
  

It should lock successfully.

  $ opam-monorepo lock vendored > /dev/null

The lockfile should thus contain the package `b` and mark it as `vendor` since
opam-monorepo will vendor it.

  $ opam show --no-lint --raw -fdepends ./vendored.opam.locked | grep "\"b\""
  "b" {= "1" & vendor}

Let's now check with the same opam file but this one adds `opam-provided`.
Asking for the value will return that `b` will be provided by opam:

  $ opam show --no-lint --raw -fx-opam-monorepo-opam-provided ./opam-provided.opam
  "b"
  $ opam-monorepo lock opam-provided > /dev/null

We should be seing that this package is not marked as `vendor` so if we run
`opam` on it, it will install the package `b`.

  $ opam show --no-lint --raw -fdepends ./opam-provided.opam.locked | grep "\"b\""
  "b" {= "1"}

What happens in the case that a package would be ok to vendor but the
transitive dependency is opam-provided? In this case we have a package
`transitive` that depends on a package that depends on a package that will
depend transitively on an opam provided package.

  $ opam show --no-lint --raw -fdepends ./transitive.opam
  "dune" "depends-on-b"
  $ opam show --no-lint --raw -fx-opam-monorepo-opam-provided ./transitive.opam
  "b"

Locking it should work as usual

  $ opam-monorepo lock transitive > /dev/null
  $ opam show --no-lint --raw -fdepends ./transitive.opam.locked | grep "b\""
  "b" {= "1"}
  "depends-on-b" {= "1" & vendor}

`depends-on-b` is vendored (since it can) but `b` is opam-provided. Neat.

Now for the reverse case, `depends-on-b` is opam-provided.

  $ opam show --no-lint --raw -fdepends ./reverse-transitive.opam
  "dune" "depends-on-b"
  $ opam show --no-lint --raw -fx-opam-monorepo-opam-provided ./reverse-transitive.opam
  "depends-on-b"

Since it is, we need to make its dependency, `b` also opam-provided.

  $ opam-monorepo lock reverse-transitive > /dev/null
  $ opam show --no-lint --raw -fdepends ./reverse-transitive.opam.locked | grep "b\""
  "b" {= "1"}
  "depends-on-b" {= "1"}
