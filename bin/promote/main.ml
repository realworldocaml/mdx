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

let on_file file ~f =
  let ic = open_in_bin file in
  let res = f ic in
  close_in ic;
  res

let read_file file =
  on_file file ~f:(fun ic ->
      let len = in_channel_length ic in
      really_input_string ic len
    )

let read_lines_of_file file ~first ~last =
  let rec aux acc ic l =
    try
      let line_read = input_line ic in
      if l < first then
        aux acc ic (l+1)
      else if l = last then
        (line_read :: acc)
      else
        aux (line_read :: acc) ic (l+1)
    with End_of_file ->
      let msg =
        Format.sprintf "End of file %s before reading line %i" file l
      in
      print_endline msg;
      assert false
  in
  List.rev (on_file file ~f:(fun ic -> aux [] ic 1))

let update_block_with_file ppf t file lines =
  Block.pp_header ppf t;
  (match lines with
  | Some lines ->
     (match Astring.String.cut ~sep:"-" lines with
     | Some (first, last) ->
        (match Astring.String.to_int first, Astring.String.to_int last with
         | Some first, Some last ->
            if first <= last then
              (match read_lines_of_file file ~first ~last with
               | [] -> assert false (*TODO: nice message *)
               | lines ->
                  let contents = Astring.String.concat ~sep:"\n" lines in
                  Output.pp ppf (`Output contents))
            else
              assert false (* TODO: nice message *)
         | None, _ -> assert false (* TODO: nice message *)
         | _, None -> assert false (* TODO: nice message *))
     | None ->
        (* lines is actually a single line *)
        (match Astring.String.to_int lines with
         | Some line ->
            (match read_lines_of_file file ~first:line ~last:line with
             | [contents] ->
                Output.pp ppf (`Output contents)
             | _ -> assert false (* TODO: nice message *))
         | None -> assert false (* TODO: nice message *))
     )
  | None ->
     let contents = read_file file in
     Output.pp ppf (`Output contents)
  );
  Block.pp_footer ppf ()

let run ()
    _non_deterministic not_verbose silent verbose_findlib prelude prelude_str
    file section _root
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

  Mdx.run file ~f:(fun file_contents items ->
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
                update_block_with_file ppf t file (Block.lines t)
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
  let doc = "Test markdown files." in
  Term.(pure run
        $ Cli.setup $ Cli.non_deterministic $ Cli.not_verbose
        $ Cli.silent $ Cli.verbose_findlib $ Cli.prelude $ Cli.prelude_str
        $ Cli.file $ Cli.section $ Cli.root),
  Term.info "mdx-promote" ~version:"%%VERSION%%" ~doc ~exits ~man

let main () = Term.(exit_status @@ eval cmd)

let () = main ()
