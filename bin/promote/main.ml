(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
 *                    Guillaume Petiot <guillaume@tarides.com>
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

open Mdx

let src = Logs.Src.create "cram.promote"
module Log = (val Logs.src_log src : Logs.LOG)

let read_lines file =
  let ic = open_in file in
  let r = ref [] in
  try while true do r := input_line ic :: !r done; assert false
  with End_of_file ->
    close_in ic;
    List.rev !r

let eval_test c test =
  Log.debug (fun l ->
      l "eval_test %a" Fmt.(Dump.list string) (Toplevel.command test));
  Mdx_top.eval c (Toplevel.command test)

let eval_raw c ~line lines =
  let t = Toplevel.{vpad=0; hpad=0; line; command = lines; output = [] } in
  let _ = eval_test c t in
  ()

let part_from_file ~file ~part =
  let open Ocaml_topexpect in
  let lexbuf = Lexbuf.of_file file in
  let v = Phrase.read_all lexbuf in
  let doc = Phrase.document lexbuf v ~matched:true in
  let parts = Document.parts doc in
  match part with
  | Some part ->
     (match List.find_opt (fun p -> String.equal (Part.name p) part) parts with
      | Some p ->
         Part.chunks p |> List.rev_map Chunk.code |> List.rev
      | None ->
         Fmt.failwith "Part %s not found in file %s" part file)
  | None ->
     List.fold_left (fun acc p ->
         let chunks = Part.chunks p |> List.rev_map Chunk.code in
         List.fold_left (fun acc x -> x :: acc) ("" :: acc) chunks
       ) [] parts |> List.rev

let update_block_with_file ppf t file part =
  Block.pp_header ppf t;
  let lines = part_from_file ~file ~part in
  let contents = Astring.String.concat ~sep:"\n" lines in
  Output.pp ppf (`Output contents);
  Block.pp_footer ppf ()

let update_file_with_block ppf t file part =
  let open Ocaml_topexpect in
  let lexbuf = Lexbuf.of_file file in
  let v = Phrase.read_all lexbuf in
  let doc = Phrase.document lexbuf v ~matched:true in
  let parts = Document.parts doc in
  (match part with
   | Some part ->
      let on_part p =
        let name = Part.name p in
        let lines =
          if String.equal name part then
            match Block.value t with
            | Raw | OCaml | Error _ -> t.Block.contents
            | Cram _ ->
               Fmt.failwith "Promoting Cram tests is unsupported for now."
            | Toplevel tests ->
               let f t =
                 t.Toplevel.command |> Astring.String.concat ~sep:"\n" in
               List.map f tests
          else
            Part.chunks p |> List.rev_map Chunk.code |> List.rev
        in
        if String.equal name "" then lines
        else ("[@@@part \"" ^ name ^ "\"];;") :: lines
      in
      let lines = List.map on_part parts in
      let lines = List.map (Astring.String.concat ~sep:"\n") lines in
      let lines = Astring.String.concat ~sep:"\n" lines in
      let oc = open_out file in
      output_string oc lines;
      close_out oc;
   | None -> () );
  Block.pp ppf t

let run ()
    _ not_verbose silent verbose_findlib prelude prelude_str
    md_file section _
  =
  let c =
    Mdx_top.init ~verbose:(not not_verbose) ~silent ~verbose_findlib ()
  in
  let section = match section with
    | None   -> None
    | Some p -> Some (Re.Perl.compile_pat p)
  in
  let active b = match section, Block.section b with
    | None   , _      -> true
    | Some re, None   -> Re.execp re ""
    | Some re, Some s -> Re.execp re (snd s)
  in
  let () = match prelude, prelude_str with
    | None  , None   -> ()
    | Some f, None   -> eval_raw c ~line:0 (read_lines f)
    | None  , Some f -> eval_raw c ~line:0 [f]
    | Some _, Some _ ->
      Fmt.failwith "only one of --prelude and --prelude-str shoud be used"
  in
  Mdx.run md_file ~f:(fun file_contents items ->
      let temp_file = Filename.temp_file "mdx" ".output" in
      at_exit (fun () -> Sys.remove temp_file);
      let buf = Buffer.create (Astring.String.length file_contents + 1024) in
      let ppf = Format.formatter_of_buffer buf in
      List.iter (function
          | Section _
          | Text _ as t -> Mdx.pp_line ppf t
          | Block t when not (active t) -> Block.pp ppf t
          | Block t ->
             match Block.file t with
             | Some file ->
                let md_file_mtime = (Unix.stat md_file).st_mtime in
                let file_mtime = (Unix.stat file).st_mtime in
                if file_mtime < md_file_mtime then
                  ( print_endline ("MD file is more recent");
                    update_file_with_block ppf t file (Block.part t) )
                else
                  ( print_endline ("ML file is more recent");
                    update_block_with_file ppf t file (Block.part t) )
             | None ->
                (* Lines are ignored if file is not specified. *)
                Block.pp ppf t
        ) items;
      Format.pp_print_flush ppf ();
      Buffer.contents buf);
  0

(**** Cmdliner ****)

open Cmdliner

let cmd =
  let exits = Term.default_exits in
  let man = [] in
  let doc = "Promote markdown files." in
  Term.(pure run
        $ Cli.setup $ Cli.non_deterministic $ Cli.not_verbose
        $ Cli.silent $ Cli.verbose_findlib $ Cli.prelude $ Cli.prelude_str
        $ Cli.file $ Cli.section $ Cli.root),
  Term.info "mdx-promote" ~version:"%%VERSION%%" ~doc ~exits ~man

let main () = Term.(exit_status @@ eval cmd)

let () = main ()
