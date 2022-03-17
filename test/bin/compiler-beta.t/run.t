We have a simple project that only depends on OCaml

  $ cat test.opam
  opam-version: "2.0"
  depends: [
    "ocaml"
    "dune"
  ]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo"
    "file://$OPAM_MONOREPO_CWD/repo-with-beta-ocaml"
  ]

We use the minimal repository which includes OCaml 4.13.1

  $ gen-minimal-repo

We also use an extra repository which happens to contain the packages of the
ongoing beta release of OCaml 4.14.1

  $ cat repo-with-beta-ocaml/packages/ocaml/ocaml.4.14.0/opam
  opam-version: "2.0"
  license: "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception"
  synopsis: "The OCaml compiler (virtual package)"
  description: """
  This package requires a matching implementation of OCaml,
  and polls it to initialise specific variables like `ocaml:native-dynlink`"""
  maintainer: "platform@lists.ocaml.org"
  depends: [
    "ocaml-config" {>= "2"}
    "ocaml-base-compiler" {>= "4.14.0~" & < "4.14.1~"} |
    "ocaml-variants" {>= "4.14.0~" & < "4.14.1~"} |
    "ocaml-system" {>= "4.14.0" & < "4.14.1~"}
  ]
  setenv: [
    [CAML_LD_LIBRARY_PATH = "%{_:stubsdir}%"]
    [CAML_LD_LIBRARY_PATH += "%{lib}%/stublibs"]
    [OCAML_TOPLEVEL_PATH = "%{toplevel}%"]
  ]
  build: ["ocaml" "%{ocaml-config:share}%/gen_ocaml_config.ml" _:version _:name]
  build-env: CAML_LD_LIBRARY_PATH = ""
  homepage: "https://ocaml.org"
  bug-reports: "https://github.com/ocaml/opam-repository/issues"
  authors: [
    "Xavier Leroy"
    "Damien Doligez"
    "Alain Frisch"
    "Jacques Garrigue"
    "Didier Rémy"
    "Jérôme Vouillon"
  ]
  flags: conf
  $ cat repo-with-beta-ocaml/packages/ocaml-base-compiler/ocaml-base-compiler.4.14.0~beta1/opam
  opam-version: "2.0"
  synopsis: "First beta release of OCaml 4.14.0"
  maintainer: "platform@lists.ocaml.org"
  license: "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception"
  authors: "Xavier Leroy and many contributors"
  homepage: "https://ocaml.org"
  bug-reports: "https://github.com/ocaml/opam-repository/issues"
  dev-repo: "git+https://github.com/ocaml/ocaml#4.14"
  depends: [
    "ocaml" {= "4.14.0" & post}
    "base-unix" {post}
    "base-bigarray" {post}
    "base-threads" {post}
    "ocaml-options-vanilla" {post}
    "ocaml-beta" {opam-version < "2.1.0"}
  ]
  conflict-class: "ocaml-core-compiler"
  flags: [ compiler avoid-version ]
  setenv: CAML_LD_LIBRARY_PATH = "%{lib}%/stublibs"
  build: [
    [
      "./configure"
      "--prefix=%{prefix}%"
      "--docdir=%{doc}%/ocaml"
      "-C"
      "CC=cc" {os = "openbsd" | os = "macos"}
      "ASPP=cc -c" {os = "openbsd" | os = "macos"}
    ]
    [make "-j%{jobs}%"]
  ]
  install: [make "install"]
  url {
    src: "https://github.com/ocaml/ocaml/archive/4.14.0-beta1.tar.gz"
    checksum: "sha256=1b02000867fd20af59c2afa1eda411d064f02cf96227f7f3e07fbeb5492abe95"
  }
  extra-files: ["ocaml-base-compiler.install" "md5=3e969b841df1f51ca448e6e6295cb451"]
  post-messages: [
    "A failure in the middle of the build may be caused by build parallelism
     (enabled by default).
     Please file a bug report at https://github.com/ocaml/opam-repository/issues"
    {failure & jobs > 1}
    "You can try installing again including --jobs=1
     to force a sequential build instead."
    {failure & jobs > 1 & opam-version >= "2.0.5"}
  ]

Our package does not define an upper bound on ocaml but the beta should not be
selected by default if there exist another solution, therefore here the solver
should pick 4.13.1:

  $ opam-monorepo lock test > /dev/null
  $ opam show --no-lint -fdepends ./test.opam.locked | grep ocaml
  "ocaml" {= "4.14.0"}
  "ocaml-base-compiler" {= "4.14.0~beta1"}
  "ocaml-config" {= "2"}
  "ocaml-options-vanilla" {= "1"}

Now if we require ocaml >= 4.14, as the following package does:

  $ cat test-requires-beta.opam
  opam-version: "2.0"
  depends: [
    "ocaml" {>= "4.14"}
    "dune"
  ]
  x-opam-monorepo-opam-repositories: [
    "file://$OPAM_MONOREPO_CWD/minimal-repo"
    "file://$OPAM_MONOREPO_CWD/repo-with-beta-ocaml"
  ]

The solver should be able to select the beta compiler since it is the only
satisfying version available:

  $ opam-monorepo lock test-requires-beta > /dev/null
  $ opam show --no-lint -fdepends ./test-requires-beta.opam.locked | grep ocaml
  "ocaml" {= "4.14.0"}
  "ocaml-base-compiler" {= "4.14.0~beta1"}
  "ocaml-config" {= "2"}
  "ocaml-options-vanilla" {= "1"}
