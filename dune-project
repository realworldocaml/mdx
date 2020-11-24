(lang dune 2.0)
(generate_opam_files true)
(name opam-monorepo)

(source (github ocamllabs/duniverse))
(license ISC)
(authors "Anil Madhavapeddy" "Nathan Rebours" "Lucas Pluvinage" "Jules Aguillon")
(maintainers "anil@recoil.org")

(package
 (name opam-monorepo)
 (synopsis "Assemble and manage fully vendored Dune repositories")
 (description "The opam monorepo plugin provides a convenient interface to bridge the
opam package manager with having a local copy of all the source
code required to build a project using the dune build tool.")
 (depends
  (ocaml (>= 4.08.0))
  dune-build-info
  bos
  cmdliner
  fmt
  logs
  (opam-file-format (>= 2.1.0~beta))
  (opam-format (>= 2.1.0~beta))
  (opam-state (>= 2.1.0~beta))
  opam-0install
  ocaml-version
  sexplib
  uri
  (alcotest :with-test)
))
