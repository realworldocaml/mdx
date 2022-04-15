We have a simple project with a single package defined at the root.
It has a `x-opam-monorepo-opam-repositories` field set to use a local
opam-repository for locking

  $ cat simple-lock.opam
  opam-version: "2.0"
  depends: [
    "dune"
    "b"
    "c"
  ]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo"
    "file://$OPAM_MONOREPO_CWD/repo"
  ]

We provided a minimal opam-repository but locking should be successful.

  $ gen-minimal-repo
  $ opam-monorepo lock
  ==> Using 1 locally scanned package as the target.
  ==> Found 10 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  ==> Wrote lockfile with 2 entries to $TESTCASE_ROOT/simple-lock.opam.locked. You can now run opam monorepo pull to fetch their sources.

The lockfile should contain the base packages, dune and our 2 dependencies
`b` and `c` which should be pulled in the duniverse

  $ cat simple-lock.opam.locked
  opam-version: "2.0"
  synopsis: "opam-monorepo generated lockfile"
  maintainer: "opam-monorepo"
  depends: [
    "b" {= "1" & ?vendor}
    "base-bigarray" {= "base"}
    "base-threads" {= "base"}
    "base-unix" {= "base"}
    "c" {= "1" & ?vendor}
    "dune" {= "2.9.1"}
    "ocaml" {= "4.13.1"}
    "ocaml-base-compiler" {= "4.13.1"}
    "ocaml-config" {= "2"}
    "ocaml-options-vanilla" {= "1"}
  ]
  pin-depends: [
    ["b.1" "https://b.com/b.tbz"]
    ["c.1" "https://c.com/c.tbz"]
  ]
  x-opam-monorepo-duniverse-dirs: [
    [
      "https://b.com/b.tbz"
      "b"
      [
        "sha256=0000000000000000000000000000000000000000000000000000000000000000"
      ]
    ]
    [
      "https://c.com/c.tbz"
      "c"
      [
        "sha256=0000000000000000000000000000000000000000000000000000000000000001"
      ]
    ]
  ]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo" "file://$OPAM_MONOREPO_CWD/repo"
  ]
  x-opam-monorepo-root-packages: ["simple-lock"]
  x-opam-monorepo-version: "0.3"
