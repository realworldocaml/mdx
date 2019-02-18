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
open Compat
open Result

let src = Logs.Src.create "cram.test"
module Log = (val Logs.src_log src : Logs.LOG)

let (/) = Filename.concat

let prelude_env_and_file f =
  match Astring.String.cut ~sep:":" f with
  | None        -> None, f
  | Some (e, f) ->
    if Astring.String.exists ((=) ' ') e
    then None  , f
    else Some e, f

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

let root_dir ?root t =
  match root, Mdx.Block.directory t with
  | None  , None   -> None
  | None  , Some d -> Some (Filename.dirname t.file / d)
  | Some r, Some d -> Some (r / d)
  | Some d, None   -> Some d

let run_cram_tests ?syntax t ?root ppf temp_file pad tests =
  Block.pp_header ?syntax ppf t;
  let pad =
    match syntax with
    | Some Cram -> pad + 2
    | _ -> pad
  in
  List.iter (fun test ->
      let root = root_dir ?root t in
      let n = run_test ?root temp_file test in
      let lines = read_lines temp_file in
      let output =
        let output = List.map (fun x -> `Output x) lines in
        if Output.equal output test.output then test.output
        else Output.merge output test.output
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
  Block.pp_footer ?syntax ppf ()

let eval_test t ?root c test =
  Log.debug (fun l ->
      l "eval_test %a" Fmt.(Dump.list (Fmt.fmt "%S")) (Toplevel.command test));
  let root = root_dir ?root t in
  with_dir root (fun () -> Mdx_top.eval c (Toplevel.command test))

let err_eval ~cmd lines =
    Fmt.epr "Got an error while evaluating:\n---\n%a\n---\n%a\n%!"
      Fmt.(list ~sep:(unit "\n") string) cmd
      Fmt.(list ~sep:(unit "\n") string) lines;
    exit 1

let eval_raw t ?root c ~line lines =
  let test = Toplevel.{vpad=0; hpad=0; line; command = lines; output = [] } in
  match eval_test t ?root c test with
  | Ok _    -> ()
  | Error e -> err_eval ~cmd:lines e

let lines = function Ok x | Error x -> x

let split_lines lines =
  let aux acc s =
    (* XXX(samoht) support windowns *)
    let lines = String.split_on_char '\n' s in
    List.append lines acc
  in
  List.fold_left aux [] (List.rev lines)

let run_toplevel_tests ?root c ppf tests t =
  Block.pp_header ppf t;
  List.iter (fun test ->
      let lines = lines (eval_test ?root t c test) in
      let lines = split_lines lines in
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

let trim l =
  let rec aux = function
    | []   -> []
    | h::t -> if String.trim h = "" then aux t else  String.trim h :: t
  in
  let no_head = aux l in
  let no_tail = List.rev (aux (List.rev no_head)) in
  no_tail

type file = { first: Mdx_top.Part.file; current: Mdx_top.Part.file }

let files: (string, file) Hashtbl.t = Hashtbl.create 8

let has_changed { first; current } =
  let contents = Mdx_top.Part.contents current in
  if contents = Mdx_top.Part.contents first
  then None
  else Some contents

let read_parts file =
  try Hashtbl.find files file
  with Not_found ->
    let parts = Mdx_top.Part.read file in
    let f = { first=parts; current=parts} in
    Hashtbl.add files file f;
    f

let write_parts file parts =
  let output_file = file ^ ".corrected" in
  match has_changed parts with
  | None   -> if Sys.file_exists output_file then Sys.remove output_file
  | Some c ->
    let oc = open_out output_file in
    output_string oc c;
    flush oc;
    close_out oc

let update_block_with_file ppf t file part =
  Block.pp_header ppf t;
  let parts = read_parts file in
  match Mdx_top.Part.find parts.current ~part with
  | None       ->
    Fmt.failwith "Cannot find part %S in %s"
      (match part with None -> "" | Some p -> p)
      file
  | Some lines ->
    let lines = trim lines in
    let contents = String.concat "\n" lines in
    Output.pp ppf (`Output contents);
    Block.pp_footer ppf ()

let update_file_with_block ppf t file part =
  let parts = read_parts file in
  let lines =
    match Block.value t with
    | Raw | OCaml | Error _ -> t.Block.contents
    | Cram _ ->
      Fmt.failwith "Promoting Cram tests is unsupported for now."
    | Toplevel tests ->
      let f t =
        t.Toplevel.command |> String.concat "\n\n" in
      List.map f tests
  in
  let current = Mdx_top.Part.replace parts.current ~part ~lines in
  let parts = { parts with current } in
  Hashtbl.replace files file parts;
  Block.pp ppf t

let update_file_or_block ?root ppf md_file ml_file block direction =
  let root = root_dir ?root block in
  let dir = Filename.dirname md_file in
  let ml_file = match root with
    | None   -> dir / ml_file
    | Some r -> r / dir / ml_file
  in
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

let run_exn ()
    non_deterministic not_verbose syntax silent verbose_findlib prelude
    prelude_str file section root direction
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
    | [], [] -> ()
    | [], fs ->
      List.iter (fun p ->
          let env, f = prelude_env_and_file p in
          let eval () = eval_raw Block.empty ?root c ~line:0 [f] in
          match env with
          | None   -> eval ()
          | Some e -> Mdx_top.in_env e eval
        ) fs
    | fs, [] ->
      List.iter (fun p ->
          let env, f = prelude_env_and_file p in
          let eval () = eval_raw Block.empty ?root c ~line:0 (read_lines f) in
          match env with
          | None   -> eval ()
          | Some e -> Mdx_top.in_env e eval
        ) fs
    | _ -> Fmt.failwith "only one of --prelude or --prelude-str shoud be used"
  in

  Mdx.run ?syntax file ~f:(fun file_contents items ->
      let temp_file = Filename.temp_file "mdx" ".output" in
      at_exit (fun () -> Sys.remove temp_file);
      let buf = Buffer.create (String.length file_contents + 1024) in
      let ppf = Format.formatter_of_buffer buf in
      List.iter (function
          | Section _
          | Text _ as t -> Mdx.pp_line ?syntax ppf t
          | Block t ->
            Mdx_top.in_env (Block.environment t)
              (fun () ->
                 let active = active t && (not (Block.skip t)) in
                 match active, non_deterministic, Block.mode t, Block.value t with
                 (* Print errors *)
                 | _, _, _, Error _ -> Block.pp ?syntax ppf t
                 (* Skip raw blocks. *)
                 | true, _, _, Raw -> Block.pp ?syntax ppf t
                 (* The command is not active, skip it. *)
                 | false, _, _, _ -> Block.pp ?syntax ppf t
                 (* the command is active but non-deterministic so skip everything *)
                 | true, false, `Non_det `Command, _ -> Block.pp ?syntax ppf t
                 (* the command is active but it's output is
                    non-deterministic; run it but keep the old output. *)
                 | true, false, `Non_det `Output, Cram { tests; _ } ->
                   Block.pp ?syntax ppf t;
                   List.iter (fun t ->
                       let _: int = run_test ?root temp_file t in ()
                     ) tests
                 | true, false, `Non_det `Output, Toplevel tests ->
                   assert (syntax <> Some Cram);
                   Block.pp ppf t;
                   List.iter (fun test ->
                       match eval_test t ?root c test with
                       | Ok _    -> ()
                       | Error e ->
                         let output = List.map (fun l -> `Output l) e in
                         if Output.equal test.output output then ()
                         else err_eval ~cmd:test.command e
                     ) tests
                 (* Run raw OCaml code *)
                 | true, _, _, OCaml ->
                   assert (syntax <> Some Cram);
                   let version_enabled = Block.version_enabled t in
                   (match Block.file t with
                    | Some ml_file when version_enabled ->
                      update_file_or_block ?root ppf file ml_file t direction
                    | None when version_enabled ->
                      eval_raw t ?root c ~line:t.line t.contents;
                      Block.pp ppf t
                    | _ -> Block.pp ppf t )
                 (* Cram tests. *)
                 | true, _, _, Cram { tests; pad } ->
                   run_cram_tests ?syntax t ?root ppf temp_file pad tests
                 (* Top-level tests. *)
                 | true, _, _, Toplevel tests ->
                   assert (syntax <> Some Cram);
                   let version_enabled = Block.version_enabled t in
                   match Block.file t with
                   | Some ml_file when version_enabled ->
                     update_file_or_block ?root ppf file ml_file t direction
                   | None when version_enabled ->
                     run_toplevel_tests ?root c ppf tests t
                   | _ -> Block.pp ppf t
              )
        ) items;
      Format.pp_print_flush ppf ();
      Buffer.contents buf);
  Hashtbl.iter write_parts files;
  0

let run ()
    non_deterministic not_verbose syntax silent verbose_findlib prelude
    prelude_str file section root direction
  =
    try
    run_exn () non_deterministic not_verbose syntax silent verbose_findlib
      prelude prelude_str file section root direction
    with Failure f -> prerr_endline f; exit 1
 
(**** Cmdliner ****)

open Cmdliner

let cmd =
  let exits = Term.default_exits in
  let man = [] in
  let doc = "Test markdown files." in
  Term.(pure run
        $ Cli.setup $ Cli.non_deterministic $ Cli.not_verbose $ Cli.syntax
        $ Cli.silent $ Cli.verbose_findlib $ Cli.prelude $ Cli.prelude_str
        $ Cli.file $ Cli.section $ Cli.root $ Cli.direction),
  Term.info "mdx-test" ~version:"%%VERSION%%" ~doc ~exits ~man

let main () = Term.(exit_status @@ eval cmd)

let () = main ()
