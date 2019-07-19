(lang dune 1.11)
(using fmt 1.1)
(generate_opam_files true)
(name duniverse)

(source (github avsm/duniverse))
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
  (ppx_sexp_conv (>= v0.9.0))
  bos
  cmdliner
  fmt
  logs
  rresult
  opam-file-format
  uri
  sexplib
  opam-core
  (alcotest :with-test)
))
