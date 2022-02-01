opam-monorepo looks for local packages definition recursively.
We want to make sure that it does not search for them in some specific places
such as the _build, _opam and duniverse directories.
We also want to make sure it is possible to define an opam-repository
locally and that it won't treat the packages defined in there as local.

In this project we have defined packages in each of those type of folders
plus a package called root at the root and one called some-local-pkg in a
sub dir.

Running opam monorepo lock --recurse should only pick those last two as targets

  $ opam-monorepo lock --lockfile test.opam.locked --recurse | grep locally
  ==> Using 2 locally scanned packages as the targets.
  $ cat test.opam.locked | grep root-packages
  x-opam-monorepo-root-packages: ["root" "some-local-pkg"]
