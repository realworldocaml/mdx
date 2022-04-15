With opam-0install solver, the context is responsible for defining the
preference order between versions of a package.
To stick to the default behaviour of the switch based context,
the repository list based context should properly sort the candidates so
that the solver picks the highest satisfying version.

We setup the default base repository

  $ gen-minimal-repo

Here we define a package higher-version that depends on a package `a`:

  $ cat higher-version.opam
  opam-version: "2.0"
  depends: [
    "dune"
    "a"
  ]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo"
    "file://$OPAM_MONOREPO_CWD/repo"
  ]

We have a local repo that defines two versions of `a`, both being satisfying
solutions:

  $ cat repo/packages/a/a.0.1/opam
  opam-version: "2.0"
  dev-repo: "git+https://a.com/a.git"
  depends: [
    "dune"
  ]
  url {
    src: "https://a.com/a.0.1.tbz"
    checksum: [
      "sha256=0000000000000000000000000000000000000000000000000000000000000000"
    ]
  }
  $ cat repo/packages/a/a.0.2/opam
  opam-version: "2.0"
  dev-repo: "git+https://a.com/a.git"
  depends: [
    "dune"
  ]
  url {
    src: "https://a.com/a.0.2.tbz"
    checksum: [
      "sha256=0000000000000000000000000000000000000000000000000000000000000001"
    ]
  }

opam-monorepo solver should pick a.0.2 here:

  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  ==> Found 9 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  ==> Wrote lockfile with 1 entries to $TESTCASE_ROOT/higher-version.opam.locked. You can now run opam monorepo pull to fetch their sources.
  $ cat higher-version.opam.locked | grep "\"a\"\s\+{"
    "a" {= "0.2" & ?vendor}
