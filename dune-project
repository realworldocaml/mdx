(lang dune 2.7)
(generate_opam_files true)
(name opam-monorepo)

(cram enable)

(source (github ocamllabs/opam-monorepo))
(license ISC)
(authors "Anil Madhavapeddy" "Nathan Rebours" "Lucas Pluvinage" "Jules Aguillon")
(maintainers "anil@recoil.org")
(documentation "https://ocamllabs.github.io/opam-monorepo")

(package
 (name opam-monorepo)
 (synopsis "Assemble and manage fully vendored Dune repositories")
 (description "The opam monorepo plugin provides a convenient interface to bridge the
opam package manager with having a local copy of all the source
code required to build a project using the dune build tool.")
 (depends
  (ocaml (>= 4.08.0))
  dune-build-info
  base
  bos
  (cmdliner (>= 1.1.0))
  fmt
  logs
  (opam-file-format (>= 2.1.0))
  (opam-format (>= 2.1.0~rc2))
  (opam-state (>= 2.1.0~rc2))
  opam-0install
  ocaml-version
  sexplib
  uri
  (alcotest :with-test))
 (conflicts
  (dune-build-info (or (= 2.7.0) (= 2.7.1)))
  (dune-configurator (or (= 2.7.0) (= 2.7.1)))))
