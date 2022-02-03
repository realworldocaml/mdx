let write_pkg ~name ~version ~opam packages_dir =
  let open OpamFilename.Op in
  let dir = packages_dir / name in
  OpamFilename.mkdir dir;
  let vdir = dir / Printf.sprintf "%s.%s" name version in
  OpamFilename.mkdir vdir;
  let opam_file = OpamFile.make (vdir // "opam") in
  OpamFile.OPAM.write opam_file opam

let write_base_bigarray packages_dir =
  let opam =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
maintainer: "https://github.com/ocaml/opam-repository/issues"
description: """
Bigarray library distributed with the OCaml compiler
"""
|}
  in
  write_pkg ~name:"base-bigarray" ~version:"base" ~opam packages_dir

let write_base_threads packages_dir =
  let opam =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
maintainer: "https://github.com/ocaml/opam-repository/issues"
description: """
Threads library distributed with the OCaml compiler
"""
|}
  in
  write_pkg ~name:"base-threads" ~version:"base" ~opam packages_dir

let write_base_unix packages_dir =
  let opam =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
maintainer: "https://github.com/ocaml/opam-repository/issues"
description: """
Unix library distributed with the OCaml compiler
"""
|}
  in
  write_pkg ~name:"base-unix" ~version:"base" ~opam packages_dir

let write_dune packages_dir =
  let opam =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
synopsis: "Fast, portable, and opinionated build system"
description: """

dune is a build system that was designed to simplify the release of
Jane Street packages. It reads metadata from "dune" files following a
very simple s-expression syntax.

dune is fast, has very low-overhead, and supports parallel builds on
all platforms. It has no system dependencies; all you need to build
dune or packages using dune is OCaml. You don't need make or bash
as long as the packages themselves don't use bash explicitly.

dune supports multi-package development by simply dropping multiple
repositories into the same directory.

It also supports multi-context builds, such as building against
several opam roots/switches simultaneously. This helps maintaining
packages across several versions of OCaml and gives cross-compilation
for free.
"""
maintainer: ["Jane Street Group, LLC <opensource@janestreet.com>"]
authors: ["Jane Street Group, LLC <opensource@janestreet.com>"]
license: "MIT"
homepage: "https://github.com/ocaml/dune"
doc: "https://dune.readthedocs.io/"
bug-reports: "https://github.com/ocaml/dune/issues"
conflicts: [
  "merlin" {< "3.4.0"}
  "ocaml-lsp-server" {< "1.3.0"}
  "dune-configurator" {< "2.3.0"}
  "odoc" {< "1.3.0"}
  "dune-release" {< "1.3.0"}
  "js_of_ocaml-compiler" {< "3.6.0"}
  "jbuilder" {= "transition"}
]
dev-repo: "git+https://github.com/ocaml/dune.git"
build: [
  # opam 2 sets OPAM_SWITCH_PREFIX, so we don't need a hardcoded path
  ["ocaml" "configure.ml" "--libdir" lib] {opam-version < "2"}
  ["ocaml" "bootstrap.ml" "-j" jobs]
  ["./dune.exe" "build" "-p" name "--profile" "dune-bootstrap" "-j" jobs]
]
depends: [
  # Please keep the lower bound in sync with .github/workflows/workflow.yml,
  # dune-project and min_ocaml_version in bootstrap.ml
  ("ocaml" {>= "4.08"} | ("ocaml" {< "4.08~~"} & "ocamlfind-secondary"))
  "base-unix"
  "base-threads"
]
x-commit-hash: "e41c66259135d6d1d72b031be6684bf8826a2586"
url {
  src: "https://github.com/ocaml/dune/releases/download/2.9.1/dune-2.9.1.tbz"
  checksum: [
    "sha256=b374feb22b34099ccc6dd32128e18d088ff9a81837952b29f05110b308c09f26"
    "sha512=fce1aa520db785c25ded75a959e9dafeb7887d4f5deeb14b044cd5b9e2d235dca24589d794d2f01513765bc4764cf72f8659bd15f3a4fc06efa61363dc5d709b"
  ]
}
|}
  in
  write_pkg ~name:"dune" ~version:"2.9.1" ~opam packages_dir

let write_ocaml_base_compiler packages_dir =
  let opam =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
synopsis: "Official release 4.13.1"
maintainer: "platform@lists.ocaml.org"
license: "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception"
authors: "Xavier Leroy and many contributors"
homepage: "https://ocaml.org"
bug-reports: "https://github.com/ocaml/opam-repository/issues"
dev-repo: "git+https://github.com/ocaml/ocaml"
depends: [
  "ocaml" {= "4.13.1" & post}
  "base-unix" {post}
  "base-bigarray" {post}
  "base-threads" {post}
  "ocaml-options-vanilla" {post}
]
conflict-class: "ocaml-core-compiler"
flags: compiler
setenv: CAML_LD_LIBRARY_PATH = "%{lib}%/stublibs"
build: [
  [
    "./configure"
    "--prefix=%{prefix}%"
    "-C"
    "CC=cc" {os = "openbsd" | os = "macos"}
    "ASPP=cc -c" {os = "openbsd" | os = "macos"}
  ]
  [make "-j%{jobs}%"]
]
install: [make "install"]
url {
  src: "https://github.com/ocaml/ocaml/archive/4.13.1.tar.gz"
  checksum: "sha256=194c7988cc1fd1c64f53f32f2f7551e5309e44d914d6efc7e2e4d002296aeac4"
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
|}
  in
  write_pkg ~name:"ocaml-base-compiler" ~version:"4.13.1" ~opam packages_dir

let write_ocaml_config packages_dir =
  let opam =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
synopsis: "OCaml Switch Configuration"
description: """
This package is used by the OCaml package to set-up its variables."""
maintainer: "platform@lists.ocaml.org"
authors: [
  "Louis Gesbert <louis.gesbert@ocamlpro.com>"
  "David Allsopp <david.allsopp@metastack.com>"
]
homepage: "https://opam.ocaml.org/"
bug-reports: "https://github.com/ocaml/opam/issues"
depends: [
  "ocaml-base-compiler" {>= "4.12.0~"} |
  "ocaml-variants" {>= "4.12.0~"} |
  "ocaml-system" {>= "4.12.0~"}
]
substs: "gen_ocaml_config.ml"
extra-files: [
  ["gen_ocaml_config.ml.in" "md5=a4b41e3236593d8271295b84b0969172"]
  ["ocaml-config.install" "md5=8e50c5e2517d3463b3aad649748cafd7"]
]
|}
  in
  write_pkg ~name:"ocaml-config" ~version:"2" ~opam packages_dir

let write_ocaml_options_vanilla packages_dir =
  let opam =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
synopsis: "Ensure that OCaml is compiled with no special options enabled"
depends: [
  "ocaml-base-compiler" {post} |
  "ocaml-system" {post} |
  "ocaml-variants" {post & >= "4.12.0~"}
]
conflicts: [
  "ocaml-option-32bit"
  "ocaml-option-afl"
  "ocaml-option-bytecode-only"
  "ocaml-option-default-unsafe-string"
  "ocaml-option-flambda"
  "ocaml-option-fp"
  "ocaml-option-musl"
  "ocaml-option-no-flat-float-array"
  "ocaml-option-spacetime"
  "ocaml-option-static"
  "ocaml-option-nnp"
  "ocaml-option-nnpchecker"
]
maintainer: "platform@lists.ocaml.org"
flags: compiler
|}
  in
  write_pkg ~name:"ocaml-options-vanilla" ~version:"1" ~opam packages_dir

let write_ocaml packages_dir =
  let opam =
    OpamFile.OPAM.read_from_string
      {|
opam-version: "2.0"
license: "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception"
synopsis: "The OCaml compiler (virtual package)"
description: """
This package requires a matching implementation of OCaml,
and polls it to initialise specific variables like `ocaml:native-dynlink`"""
maintainer: "platform@lists.ocaml.org"
depends: [
  "ocaml-config" {>= "2"}
  "ocaml-base-compiler" {>= "4.13.1~" & < "4.13.2~"} |
  "ocaml-variants" {>= "4.13.1~" & < "4.13.2~"} |
  "ocaml-system" {>= "4.13.1" & < "4.13.2~"}
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
|}
  in
  write_pkg ~name:"ocaml" ~version:"4.13.1" ~opam packages_dir

let write_repo_file repo_dir =
  let repo_file = OpamRepositoryPath.repo repo_dir in
  let repo =
    OpamFile.Repo.create ~opam_version:(OpamVersion.of_string "2.0") ()
  in
  OpamFile.Repo.write repo_file repo

let () =
  let open OpamFilename.Op in
  let cwd = OpamFilename.cwd () in
  let minimal_repo = cwd / "minimal-repo" in
  OpamFilename.mkdir minimal_repo;
  write_repo_file minimal_repo;
  let packages_dir = OpamRepositoryPath.packages_dir minimal_repo in
  OpamFilename.mkdir packages_dir;
  write_base_bigarray packages_dir;
  write_base_threads packages_dir;
  write_base_unix packages_dir;
  write_dune packages_dir;
  write_ocaml_base_compiler packages_dir;
  write_ocaml_config packages_dir;
  write_ocaml_options_vanilla packages_dir;
  write_ocaml packages_dir;
  ()
