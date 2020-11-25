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

module Opam = struct
  type package = { name : string; version : string option }

  let default_version = "zdev"

  let explicit_version p = match p.version with Some v -> v | None -> default_version

  let package_to_opam p =
    OpamPackage.create
      (OpamPackage.Name.of_string p.name)
      (OpamPackage.Version.of_string (explicit_version p))

  let package_from_opam o =
    let name = OpamPackage.(Name.to_string (name o)) in
    let version = Some OpamPackage.(Version.to_string (version o)) in
    { name; version }

  let package_from_string name =
    match Astring.String.cut ~sep:"." name with
    | None -> { name; version = None }
    | Some (name, version) -> { name; version = Some version }

  let pp_package ppf { name; version } =
    match version with None -> Fmt.pf ppf "%s" name | Some v -> Fmt.pf ppf "%s.%s" name v

  let string_of_package pkg = Fmt.strf "%a" pp_package pkg

  let sort_uniq l = List.sort_uniq (fun a b -> String.compare a.name b.name) l
end
