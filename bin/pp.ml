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

let src = Logs.Src.create "cram.pp"
module Log = (val Logs.src_log src : Logs.LOG)

let filter_by_section section lines =
  match section with
  | None -> lines
  | Some s ->
    let re = Re.Perl.compile_pat s in
    match Mdx.filter_section re lines with
    | None -> []
    | Some lines -> lines

let run (`Setup ()) (`Files files) (`Section section) =
  let t =
    List.map (fun p ->
        (p, Mdx.parse_file Normal p |> (filter_by_section section))
      ) files
  in
  match t with
  | [] -> 1
  | _  ->
    List.iter (fun (file, blocks) ->
        List.iter (function
          | Mdx.Section _ | Text _ -> ()
          | Block b ->
            let b = Mdx.Block.eval b in
            if not (Mdx.Block.skip b) then (
              Log.debug (fun l -> l "pp: %a" Mdx.Block.dump b);
              let pp_lines = Fmt.(list ~sep:(unit "\n") string) in
              let contents = Mdx.Block.executable_contents b in
              match b.value with
              | Toplevel _ -> Fmt.pr "%a\n" pp_lines contents
              | OCaml      ->
                Fmt.pr "%a\n%a\n"
                  Mdx.Block.pp_line_directive (file, b.line)
                  pp_lines contents
              | _          -> ()
            )
        ) blocks
      ) t;
    0

open Cmdliner

let cmd =
  let doc = "Pre-process markdown files to produce OCaml code." in
  let exits = Term.default_exits in
  Term.(pure run $ Cli.setup $ Cli.files $ Cli.section),
  Term.info "pp" ~doc ~exits
