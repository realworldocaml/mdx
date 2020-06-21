(lang dune 2.0)
(name opam-0install)
(formatting disabled)

(generate_opam_files true)

(source (github talex5/opam-0install-solver))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")
(documentation "https://talex5.github.io/opam-0install-solver/")

(package
 (name opam-0install)
 (synopsis "Opam solver using 0install backend")
 (description "\
Opam's default solver is designed to maintain a set of packages
over time, minimising disruption when installing new programs and
finding a compromise solution across all packages.

In many situations (e.g. CI, local roots or duniverse builds) this
is not necessary, and we can get a solution much faster by using
a different algorithm.

This package uses 0install's solver algorithm with opam packages.
")
 (depends
  fmt
  cmdliner
  opam-state
  (ocaml (>= 4.08.0))
  0install-solver
  (opam-client :with-test)
  (opam-solver :with-test)
  (astring :with-test)
  (alcotest :with-test)))

(package
 (name opam-0install-cudf)
 (synopsis "Opam solver using 0install backend using the CUDF interface")
 (description "\
Opam's default solver is designed to maintain a set of packages
over time, minimising disruption when installing new programs and
finding a compromise solution across all packages.

In many situations (e.g. CI, local roots or duniverse builds) this
is not necessary, and we can get a solution much faster by using
a different algorithm.

This package uses 0install's solver algorithm with opam packages using
the CUDF interface.
")
 (depends
  fmt
  cmdliner
  opam-state
  cudf
  (ocaml (>= 4.08.0))
  0install-solver))
