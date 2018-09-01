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

let src = Logs.Src.create "cram.test"
module Log = (val Logs.src_log src : Logs.LOG)

let read_file file =
  let ic = open_in_bin file in
  let len = in_channel_length ic in
  let file_contents = really_input_string ic len in
  close_in ic;
  file_contents

let read_lines file =
  let ic = open_in file in
  let r = ref [] in
  try while true do r := input_line ic :: !r done; assert false
  with End_of_file ->
    close_in ic;
    List.rev !r

(* From jbuilder's stdlib *)
let ansi_color_strip str =
  let len = String.length str in
  let buf = Buffer.create len in
  let rec loop i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | '\027' -> skip (i + 1)
      | c      -> Buffer.add_char buf c; loop (i + 1)
  and skip i =
    if i = len then
      Buffer.contents buf
    else
      match str.[i] with
      | 'm' -> loop (i + 1)
      | _   -> skip (i + 1)
  in
  loop 0

let with_dir root f =
  match root with
  | None   -> f ()
  | Some d ->
    let old_d = Sys.getcwd () in
    try
      Sys.chdir d;
      let r = f () in
      Sys.chdir old_d;
      r
    with e ->
      Sys.chdir old_d;
      raise e

let run_test ?root temp_file t =
  let cmd = Cram.command_line t in
  Log.info (fun l -> l "exec: %S" cmd);
  let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
  let pid = with_dir root (fun () ->
      Unix.create_process "sh" [| "sh"; "-c"; cmd |] Unix.stdin fd fd
    ) in
  Unix.close fd;
  match snd (Unix.waitpid [] pid) with
  | WEXITED n -> n
  | _ -> 255

let run_cram_tests ?root ppf temp_file pad tests t =
  Block.pp_header ppf t;
  List.iter (fun test ->
      let root = match root, Mdx.Block.directory t with
        | Some d, _ -> (* --root always win *) Some d
        | None  , d -> d
      in
      let n = run_test ?root temp_file test in
      let lines = read_lines temp_file in
      let output =
        let output = List.map (fun x -> `Output x) lines in
        if Output.equal output test.output then test.output
        else output
      in
      Cram.pp_command ~pad ppf test;
      List.iter (function
          | `Ellipsis    -> Output.pp ~pad ppf `Ellipsis
          | `Output line ->
            let line = ansi_color_strip line in
            Output.pp ~pad ppf (`Output line)
        ) output;
      Cram.pp_exit_code ~pad ppf n;
    ) tests;
  Block.pp_footer ppf ()

let envs = Hashtbl.create 8

let rec save_summary acc s =
  let open Env in
  match s with
  | Env_value (summary, id, _) ->
     save_summary (Ident.name id :: acc) summary
  | Env_module (summary, id, _)
    | Env_class (summary, id, _)
    | Env_modtype (summary, id, _)
    | Env_cltype (summary, id, _)
    | Env_type (summary, id, _)
    | Env_functor_arg (summary, id)
    | Env_open (summary,
                #if OCAML_MAJOR >= 4 && OCAML_MINOR >= 7
                _,
                #endif
                Pident id)
    | Env_extension (summary, id, _) ->
      let acc =
        if Ident.binding_time id >= 1000
        then Ident.unique_toplevel_name id :: acc
        else acc
      in
      save_summary acc summary
  | Env_empty -> acc
  | Env_constraints (summary, _)
    | Env_open (summary,
                #if OCAML_MAJOR >= 4 && OCAML_MINOR >= 7
                _,
                #endif
                _)
    | Env_copy_types (summary, _) -> save_summary acc summary

let in_env env_name f =
  let env, names, objs =
    try Hashtbl.find envs env_name
    with Not_found ->
      let env = Compmisc.initial_env () in
      env, [], []
  in
  Toploop.toplevel_env := env;
  List.iter2 Toploop.setvalue names objs;
  let res = f () in
  let env = !Toploop.toplevel_env in
  let names = save_summary [] (Env.summary env) in
  let objs = List.map Toploop.getvalue names in
  Hashtbl.replace envs env_name (env, names, objs);
  res

let eval_test c test =
  Log.debug (fun l ->
      l "eval_test %a" Fmt.(Dump.list string) (Toplevel.command test));
  Mdx_top.eval c (Toplevel.command test)

let eval_raw c ~line lines =
  let t = Toplevel.{vpad=0; hpad=0; line; command = lines; output = [] } in
  let _ = eval_test c t in
  ()

let run_toplevel_tests c ppf tests t =
  Block.pp_header ppf t;
  List.iter (fun test ->
      let lines = eval_test c test in
      let output =
        let output = List.map (fun x -> `Output x) lines in
        if Output.equal output test.output then test.output
        else output
      in
      let pad = test.hpad in
      Toplevel.pp_command ppf test;
      List.iter (function
          | `Ellipsis    -> Output.pp ~pad ppf `Ellipsis
          | `Output line ->
            let line = ansi_color_strip line in
            Output.pp ~pad ppf (`Output line)
        ) output;
    ) tests;
  Block.pp_footer ppf ()

let update_block_with_file ppf t file part =
  Block.pp_header ppf t;
  let lines = Mdx_top.Part.find ~file ~part in
  let contents = String.concat "\n" lines in
  Output.pp ppf (`Output contents);
  Block.pp_footer ppf ()

let update_file_with_block ppf t file part =
  let output_file = file ^ ".corrected" in
  let input_file =
    if Sys.file_exists output_file then output_file
    else file
  in
  (match part with
   | Some part ->
      let lines =
        match Block.value t with
        | Raw | OCaml | Error _ -> t.Block.contents
        | Cram _ ->
           Fmt.failwith "Promoting Cram tests is unsupported for now."
        | Toplevel tests ->
           let f t =
             t.Toplevel.command |> String.concat "\n" in
           List.map f tests
      in
      let lines = Mdx_top.Part.replace ~file:input_file ~part ~lines in
      let lines = List.map (String.concat "\n") lines in
      let lines = String.concat "\n" lines in
      if String.equal lines (read_file input_file) then
        ()
      else
        let oc = open_out output_file in
        output_string oc lines;
        close_out oc
   | None -> () );
  Block.pp ppf t

let update_file_or_block ppf md_file ml_file block direction =
  let direction =
    match direction with
    | `To_md -> `To_md
    | `To_ml -> `To_ml
    | `Infer_timestamp ->
       let md_file_mtime = (Unix.stat md_file).st_mtime in
       let ml_file_mtime = (Unix.stat ml_file).st_mtime in
       if ml_file_mtime < md_file_mtime then `To_ml
       else `To_md
  in
  match direction with
  | `To_md ->
     update_block_with_file ppf block ml_file (Block.part block)
  | `To_ml ->
     update_file_with_block ppf block ml_file (Block.part block)

let run ()
    non_deterministic not_verbose silent verbose_findlib prelude prelude_str
    file section root direction
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
      let buf = Buffer.create (String.length file_contents + 1024) in
      let ppf = Format.formatter_of_buffer buf in
      List.iter (function
          | Section _
          | Text _ as t -> Mdx.pp_line ppf t
          | Block t ->
            in_env (Block.environment t)
              (fun () ->
                 match active t, non_deterministic, Block.mode t, Block.value t with
                 (* Print errors *)
                 | _, _, _, Error _ -> Block.pp ppf t
                 (* Skip raw blocks. *)
                 | true, _, _, Raw -> Block.pp ppf t
                 (* The command is not active, skip it. *)
                 | false, _, _, _ -> Block.pp ppf t
                 (* the command is active but non-deterministic so skip everything *)
                 | true, false, `Non_det `Command, _ -> Block.pp ppf t
                 (* the command is active but it''s output is
                    non-deterministic; run it but keep the old output. *)
                 | true, false, `Non_det `Output, Cram { tests; _ } ->
                   Block.pp ppf t;
                   List.iter (fun t -> let _ = run_test temp_file t in ()) tests
                 | true, false, `Non_det `Output, Toplevel tests ->
                   Block.pp ppf t;
                   List.iter (fun t -> let _ = eval_test c t in ()) tests
                 (* Run raw OCaml code *)
                 | true, _, _, OCaml ->
                   (match Block.file t with
                    | Some ml_file ->
                      update_file_or_block ppf file ml_file t direction
                    | None ->
                      eval_raw c ~line:t.line t.contents;
                      Block.pp ppf t )
                 (* Cram tests. *)
                 | true, _, _, Cram { tests; pad } ->
                   run_cram_tests ?root ppf temp_file pad tests t
                 (* Top-level tests. *)
                 | true, _, _, Toplevel tests ->
                   match Block.file t with
                   | Some ml_file ->
                     update_file_or_block ppf file ml_file t direction
                   | None ->
                     run_toplevel_tests c ppf tests t
              )
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
        $ Cli.file $ Cli.section $ Cli.root $ Cli.direction),
  Term.info "mdx-test" ~version:"%%VERSION%%" ~doc ~exits ~man

let main () = Term.(exit_status @@ eval cmd)

let () = main ()
