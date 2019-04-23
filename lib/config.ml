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
  [ "menhir";
    "num";
    "ppx_tools";
    "jbuilder";
    "dune";
    "ocamlbuild";
    "ocamlmod";
    "oasis";
    "ocamlify";
    "uchar";
    "ocaml";
    "ocaml-base-compiler";
    "ocaml-variants"
  ]

let duniverse_overlays_repo = "git://github.com/dune-universe/opam-overlays.git"

let duniverse_dir = Fpath.v ".duniverse"

let opam_lockfile = Fpath.(duniverse_dir / "opam.sxp")

let duniverse_lockfile = Fpath.(duniverse_dir / "dune.sxp")

let vendor_dir = Fpath.v "duniverse"

let duniverse_log = Fpath.v ".duniverse-log"

let vendor_branch branch = Fmt.strf "duniverse-of-%s" branch
