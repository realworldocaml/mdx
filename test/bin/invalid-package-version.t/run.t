We want to make sure the error messages are sensible, and as such if the user
picks a version that doesn't exist we want to make them aware of it.

We setup the default base repository

  $ gen-minimal-repo

Here we define a package test that depends on a package `a`:

  $ opam show --no-lint --raw -fdepends ./existing.opam
  "dune" "a"

We have a local repo that defines a package `a` that satisfies the predicate,
with a version that is valid and can be picked.

  $ cat repo/packages/a/a.0.1/opam > /dev/null

opam-monorepo solver should successfully pick a.0.1:

  $ opam-monorepo lock existing
  ==> Using 1 locally scanned package as the target.
  ==> Found 9 opam dependencies for the target package.
  ==> Querying opam database for their metadata and Dune compatibility.
  ==> Calculating exact pins for each of them.
  ==> Wrote lockfile with 1 entries to $TESTCASE_ROOT/existing.opam.locked. You can now run opam monorepo pull to fetch their sources.
  $ grep "\"a\"\s\+{" existing.opam.locked 
    "a" {= "0.1" & vendor}

Yet if we attempt to use the same package, but pick a version that doesn't
exist in our repo:

  $ opam show --no-lint --raw -fdepends ./toonew.opam
  "dune" "a" {>= "1.0"}

opam-monorepo should fail with some error code and display an error message
that there is no version of `a` that matches the constraint.

(grep appends a NUL byte at the end, hence the head call, this is not important
to the test)

  $ opam-monorepo lock toonew 2> errors
  ==> Using 1 locally scanned package as the target.
  [124]
  $ grep -Pazo "(?s)opam-monorepo: \[ERROR\].*(?=opam-monorepo)" < errors | head --bytes=-1
  opam-monorepo: [ERROR] There is no eligible version of a that matches >= 1.0

We should also produce the right error message with all the constraints when we have multiple constaints

  $ opam show --no-lint --raw -fdepends ./multiple-constraint.opam
  "dune" "depends-on-min-a" "a" {< "2.0"}
  $ opam-monorepo lock multiple-constraint 2> errors
  ==> Using 1 locally scanned package as the target.
  [124]
  $ grep -Pazo "(?s)opam-monorepo: \[ERROR\].*(?=opam-monorepo)" < errors | head --bytes=-1
  opam-monorepo: [ERROR] There is no eligible version of a that matches >= 1.0
