(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

open Cmdliner

let cmds = [ Test.cmd; Pp.cmd; Deps.cmd; Dune_gen.cmd; Jupyter.cmd ]

let main (`Setup ()) = `Help (`Pager, None)

let info =
  let doc = "Execute code in documentation files." in
  let man = [] in
  Cmd.info "ocaml-mdx" ~version:"%%VERSION%%" ~doc ~man

let default = Term.(ret (const main $ Cli.setup))
let group = Cmd.group ~default info cmds
let () = Stdlib.exit @@ Cmd.eval' group
