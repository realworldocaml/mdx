(lang dune 2.2)

(name mdx)

(source
 (github realworldocaml/mdx))

(license ISC)

(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")

(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(generate_opam_files true)

(package
 (name mdx)
 (synopsis "Executable code blocks inside markdown files")
 (description
   "`ocaml-mdx` allows to execute code blocks inside markdown files.\nThere are (currently) two sub-commands, corresponding\nto two modes of operations: pre-processing (`ocaml-mdx pp`)\nand tests (`ocaml-mdx test`).\n\nThe pre-processor mode allows to mix documentation and code,\nand to practice \"literate programming\" using markdown and OCaml.\n\nThe test mode allows to ensure that shell scripts and OCaml fragments\nin the documentation always stays up-to-date.\n\n`ocaml-mdx` is released as two binaries called `ocaml-mdx` and `mdx` which are\nthe same, mdx being the deprecated name, kept for now for compatibility.")
 (depends
  (ocaml
   (>= 4.02.3))
  ocamlfind
  fmt
  (cppo :build)
  astring
  logs
  (cmdliner
   (>= 1.0.0))
  (re
   (>= 1.7.2))
  result
  (ppxlib
   (>= 0.18.0))
  (ocaml-version
   (>= 2.3.0))
  odoc
  (lwt :with-test)
  (alcotest :with-test)))
