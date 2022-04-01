We have a simple project that only depends on OCaml

  $ opam show --no-lint -fdepends ./test.opam
  "ocaml" "dune"

We use the minimal repository which includes OCaml 4.13.1

  $ gen-minimal-repo

We also use an extra repository which contains the packages of the ongoing beta
release of OCaml 4.14.0 i.e. ocaml.4.14.0 and ocaml-base-compiler.4.14.0~beta1.
Those are the actual opam files extracted from upstream opam-repository. The
relevant part to this feature is that the ocaml-base-compiler package has the
avoid-version flag:

  $ opam show --no-lint -fflags repo-with-beta-ocaml/packages/ocaml-base-compiler/ocaml-base-compiler.4.14.0~beta1/opam
  compiler avoid-version

Our package does not define an upper bound on ocaml but the beta should not be
selected by default if there exists another solution, therefore here the solver
should pick 4.13.1:

  $ opam-monorepo lock test > /dev/null
  $ opam show --no-lint -fdepends ./test.opam.locked | grep ocaml
  "ocaml" {= "4.13.1"}
  "ocaml-base-compiler" {= "4.13.1"}
  "ocaml-config" {= "2"}
  "ocaml-options-vanilla" {= "1"}

Now if we require ocaml >= 4.14, as the following package does:

  $ opam show --no-lint -fdepends ./test-requires-beta.opam
  "ocaml" {>= "4.14"} "dune"

The solver should be able to select the beta compiler since it is the only
satisfying version available:

  $ opam-monorepo lock test-requires-beta > /dev/null
  $ opam show --no-lint -fdepends ./test-requires-beta.opam.locked | grep ocaml
  "ocaml" {= "4.14.0"}
  "ocaml-base-compiler" {= "4.14.0~beta1"}
  "ocaml-config" {= "2"}
  "ocaml-options-vanilla" {= "1"}
