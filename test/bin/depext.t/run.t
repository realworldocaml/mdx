We want to make sure picking depexts works, even if we vendor packages

We setup the default base repository

  $ gen-minimal-repo

Here we define a package test that depends on a package `b`:

  $ opam show --just-file --no-lint --raw -fdepends ./a.opam
  dune, b

Package `b` has a `depext` on a fantasy OS package. We deliberately pick a
fantasy name here to make sure the behaviour is the same on all platforms.

  $ opam show --just-file --no-lint --raw -fdepexts ./repo/packages/b/b.1/opam
  libfantasydependency

We lock and expect the OS package to be part of the locked Opam file.

  $ opam-monorepo lock a > /dev/null
  $ opam show --just-file --no-lint --raw -fdepexts ./a.opam.locked
  libfantasydependency

Then we want to make sure `depext` works. Given the fantasy package does not
exist, `depext` should not install any packages but still work successfully:

  $ opam-monorepo depext --dry-run
  ==> Using lockfile $TESTCASE_ROOT/a.opam.locked
