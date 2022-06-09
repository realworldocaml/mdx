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
open Import

let base_packages =
  [
    "jbuilder";
    "dune";
    "ocamlbuild";
    "ocamlmod";
    "oasis";
    "ocamlify";
    "ocaml";
    "ocaml-base-compiler";
    "ocaml-variants";
  ]
  |> List.map ~f:OpamPackage.Name.of_string
  |> OpamPackage.Name.Set.of_list

let compiler_package_name = OpamPackage.Name.of_string "ocaml"

let duniverse_opam_repo =
  "git+https://github.com/dune-universe/opam-overlays.git"

let dune_get = Fpath.v "dune-get"
let vendor_dir = Fpath.v "duniverse"
let pins_dir = Fpath.(vendor_dir / ".pins")
let duniverse_log = Fpath.v ".duniverse-log"
let bootstrap_dir = Fpath.v "_ocaml"
let bootstrap_src_dir = Fpath.(bootstrap_dir / "src")
let ocaml_src_dir = Fpath.(bootstrap_src_dir / "ocaml")
let dune_src_dir = Fpath.(bootstrap_src_dir / "dune")
let dune_latest_tag = "2.6.0" (* TODO get from opam metadata *)
let lockfile_ext = ".opam.locked"

(* variable to use for vendoring *)
let vendor_variable = OpamVariable.of_string "vendor"
