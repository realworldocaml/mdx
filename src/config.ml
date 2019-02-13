(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

let base_packages =
  [ "menhir"
  ; "num"
  ; "ppx_tools"
  ; "jbuilder"
  ; "dune"
  ; "ocamlbuild"
  ; "ocamlmod"
  ; "oasis"
  ; "ocamlify"
  ; "uchar"
  ; "ocaml"
  ; "ocaml-base-compiler"
  ; "ocaml-variants" ]

let duniverse_forks =
  [ ("git+http://erratique.ch/repos/uutf.git", "uutf")
  ; ("git+http://erratique.ch/repos/astring.git", "astring")
  ; ("git+http://erratique.ch/repos/asetmap.git", "asetmap")
  ; ("git+http://erratique.ch/repos/logs.git", "logs")
  ; ("git+http://erratique.ch/repos/react.git", "react")
  ; ("git+http://erratique.ch/repos/xmlm.git", "xmlm")
  ; ("git+http://erratique.ch/repos/webbrowser.git", "webbrowser")
  ; ("git+http://erratique.ch/repos/uuidm.git", "uuidm")
  ; ("git+http://erratique.ch/repos/uutf.git", "uutf")
  ; ("git+http://erratique.ch/repos/mtime.git", "mtime")
  ; ("git+http://erratique.ch/repos/ptime.git", "ptime")
  ; ("git+http://erratique.ch/repos/fmt.git", "fmt")
  ; ("git+http://erratique.ch/repos/rresult.git", "rresult")
  ; ("git+http://erratique.ch/repos/jsonm.git", "jsonm")
  ; ("git+http://erratique.ch/repos/fpath.git", "fpath")
  ; ("git+http://erratique.ch/repos/bos.git", "bos")
  ; ("git+http://erratique.ch/repos/topkg.git", "topkg")
  ; ("git+http://erratique.ch/repos/cmdliner.git", "cmdliner")
  ; ("git+https://github.com/hannesm/duration.git", "duration")
  ; ("git+https://github.com/hannesm/randomconv.git", "randomconv")
  ; ("git+https://github.com/backtracking/ocaml-hashcons.git", "ocaml-hashcons")
  ; ("git+https://github.com/backtracking/ocamlgraph.git", "ocamlgraph")
  ; ("git+https://gitlab.camlcity.org/gerd/lib-findlib.git", "lib-findlib")
  ; ("git+https://gforge.inria.fr/git/dose/dose.git", "dose3")
  ; ("git+https://github.com/ocaml/opam-file-format", "opam-file-format")
  ; ("git+https://scm.gforge.inria.fr/anonscm/git/cudf/cudf.git", "cudf")
  ; ("git+https://github.com/ocaml/opam-depext.git#2.0", "opam-depext")
  ; ("git+https://github.com/ocaml/opam.git", "opam")
  ; ("git+https://github.com/superbobry/ocaml-textwrap.git", "ocaml-textwrap")
  ; ("git://github.com/ygrek/ocaml-extlib.git", "ocaml-extlib")
  ; ("git+https://github.com/mirage/mirage-xen.git", "mirage-xen")
  ; ("git+https://github.com/mirage/mirage-entropy.git", "mirage-entropy")
  ; ("git+https://github.com/pqwy/lru.git", "lru")
  ; ("git+https://github.com/pqwy/psq.git", "psq")
  ; ("git+https://github.com/ocsigen/reactiveData.git", "reactiveData")
  ; ("git+https://github.com/mirleft/ocaml-x509.git", "ocaml-x509")
  ; ("git+https://github.com/mirleft/ocaml-asn1-combinators.git", "ocaml-asn1-combinators")
  ; ("git+https://github.com/mirleft/ocaml-nocrypto.git", "ocaml-nocrypto")
  ; ("git+https://github.com/mirleft/ocaml-tls.git", "ocaml-tls")
  ; ("git+https://github.com/inhabitedtype/ocaml-dispatch.git", "ocaml-dispatch")
  ; ("git+https://github.com/OCamlPro/ocplib-endian.git", "ocplib-endian") ]

let duniverse_branch f = Fmt.strf "duniverse-%s" f

let duniverse_dir = Fpath.v ".duniverse"

let opam_lockfile = Fpath.(duniverse_dir / "opam.sxp")

let duniverse_lockfile = Fpath.(duniverse_dir / "dune.sxp")

let vendor_dir = Fpath.v "ocaml_modules"

let duniverse_log = Fpath.v ".duniverse-log"

let vendor_branch branch = Fmt.strf "duniverse-of-%s" branch
