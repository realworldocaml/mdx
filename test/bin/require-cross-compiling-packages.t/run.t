Here we have three local repositories: one that roughly represents
opam-repository, one which corresponds to opam-overlays and one
to mirage-opam-overlays.

In this scenario we depend on a package b that does not build with dune in its
original 0.1 release. There is a 0.1+dune port of it in opam-overlays:

  $ cat opam-overlays/packages/b/b.0.1+dune/opam 
  opam-version: "2.0"
  dev-repo: "git+https://b.com/b.git"
  depends: [
    "dune"
  ]
  url {
    src: "https://dune.com/b.0.1-dune.tbz"
    checksum: "sha256=0000000000000000000000000000000000000000000000000000000000000001"
  }

This dune port cannot be cross compiled in its current form so the mirage
maintainers created a 0.1+dune+mirage port in mirage-opam-overlays:

  $ cat mirage-opam-overlays/packages/b/b.0.1+dune+mirage/opam 
  opam-version: "2.0"
  dev-repo: "git+https://b.com/b.git"
  depends: [
    "dune"
  ]
  tags: ["cross-compile"]
  url {
    src: "https://mirage.com/b.0.1-dune-mirage.tbz"
    checksum: "sha256=0000000000000000000000000000000000000000000000000000000000000002"
  }

You'll note the "cross-compile" tag that we use to mark packages that can be
cross compiled in a dune-workspace.

For testing purposes we define two opam files: a.opam and a-with-mirage.opam
that are essencially the same: they both depends on b but the latter configures
the solver to use the mirage overlays in addition with upstream and
dune-universe:

  $ opam show --just-file --no-lint -fdepends ./a.opam
  dune, b
  $ opam show --just-file --no-lint -fx-opam-monorepo-opam-repositories --raw ./a.opam
  file://$OPAM_MONOREPO_CWD/opam-repository, file://$OPAM_MONOREPO_CWD/opam-overlays
  $ opam show --just-file --no-lint -fdepends ./a-with-mirage.opam
  dune, b
  $ opam show --just-file --no-lint -fx-opam-monorepo-opam-repositories --raw ./a-with-mirage.opam
  file://$OPAM_MONOREPO_CWD/opam-repository, file://$OPAM_MONOREPO_CWD/opam-overlays, file://$OPAM_MONOREPO_CWD/mirage-opam-overlays

Until there is a new release, everything goes fine. If we don't add the mirage
overlays the solver picks the dune port as expected:

  $ opam-monorepo lock a > /dev/null
  $ opam show --just-file --no-lint -fdepends ./a.opam.locked | grep "\"b\""
  "b" {= "0.1+dune" & ?vendor}

If we add the mirage overlays, the mirage port gets picked instead as its
version is higher (+mirage):

  $ opam-monorepo lock a-with-mirage > /dev/null
  $ opam show --just-file --no-lint -fdepends ./a-with-mirage.opam.locked | grep "\"b\""
  "b" {= "0.1+dune+mirage" & ?vendor}

So far so good. Problems arise when a new release of b hits upstream. We managed
to upstream the dune port before `0.2` so the `0.2` release builds with dune.

  $ mkdir opam-repository/packages/b/b.0.2
  $ cat >opam-repository/packages/b/b.0.2/opam <<EOF
  > opam-version: "2.0"
  > dev-repo: "git+https://b.com/b.git"
  > depends: [
  >   "dune"
  > ]
  > url {
  >   src: "https://b.com/b.0.2.tbz"
  >   checksum: "sha256=0000000000000000000000000000000000000000000000000000000000000003"
  > }

Regular users of opam-monorepo will get the 0.2 version and be happy with it:

  $ opam-monorepo lock a > /dev/null
  $ opam show --just-file --no-lint -fdepends ./a.opam.locked | grep "\"b\""
  "b" {= "0.2" & ?vendor}

Mirage users on the other hand will get it as well, meaning they can't cross
compile their unikernel anymore. The solver is happy but this will cause errors
at build time for them:

  $ opam-monorepo lock a-with-mirage > /dev/null
  $ grep "\"b\"\s\+{" a-with-mirage.opam.locked
    "b" {= "0.2" & ?vendor}

We added the --require-cross-compile flag to select packages that cross compile
when available.
Here, if we don't add mirage overlays and run the solver with this flag, we
still get the latest release:

  $ opam-monorepo lock --require-cross-compile a > /dev/null
  $ opam show --just-file --no-lint -fdepends ./a.opam.locked | grep "\"b\""
  "b" {= "0.2" & ?vendor}

If we run it with mirage overlays though, it will detect that there exist
versions that cross compile and favor those instead:

  $ opam-monorepo lock --require-cross-compile a-with-mirage > /dev/null
  $ opam show --just-file --no-lint -fdepends ./a-with-mirage.opam.locked | grep "\"b\""
  "b" {= "0.1+dune+mirage" & ?vendor}

Note that if the upstream released version does cross compile, it can add the
tag to be picked instead:

  $ echo "tags: [\"cross-compile\"]" >> opam-repository/packages/b/b.0.2/opam
  $ opam-monorepo lock --require-cross-compile a-with-mirage > /dev/null
  $ opam show --just-file --no-lint -fdepends ./a-with-mirage.opam.locked | grep "\"b\""
  "b" {= "0.2" & ?vendor}
