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

