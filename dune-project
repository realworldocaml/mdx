(lang dune 2.0)
(generate_opam_files true)
(name duniverse)

(source (github ocamllabs/duniverse))
(license ISC)
(authors "Anil Madhavapeddy" "Nathan Rebours" "Lucas Pluvinage" "Jules Aguillon")
(maintainers "anil@recoil.org")

(package
 (name duniverse)
 (synopsis "Assemble and manage fully vendored Dune repositories")
 (description "The duniverse tool provides a convenient interface to bridge the
opam package manager with having a local copy of all the source
code required to build a project using the dune build tool.")
 (depends
  (ocaml (>= 4.05))
  (ppx_sexp_conv (>= v0.13.0))
  dune-private-libs
  bos
  cmdliner
  fmt
  logs
  rresult
  opam-file-format
  uri
  sexplib
  opam-core
  uri-sexp
  (alcotest :with-test)
))
