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
open Astring

let src = Logs.Src.create "cram.test"
module Log = (val Logs.src_log src : Logs.LOG)

let (/) = Filename.concat

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

let output_from_line s =
  `Output (String.drop ~rev:true ~sat:Char.Ascii.is_blank s)

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

let get_env blacklist =
  let blacklist = "INSIDE_DUNE"::blacklist in
  let env = Array.to_list (Unix.environment ()) in
  let env = List.map (String.cuts ~sep:"=") env in
  let f env var =
    let g l = String.compare (List.nth l 0) var <> 0 in
    List.filter g env
  in
  let env = List.fold_left f env blacklist in
  Array.of_list (List.map (String.concat ~sep:"=") env)

let run_test ?root blacklist temp_file t =
  let cmd = Cram.command_line t in
  let env = get_env blacklist in
  Log.info (fun l -> l "exec: %S" cmd);
  let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
  let pid = with_dir root (fun () ->
      Unix.create_process_env "sh" [| "sh"; "-c"; cmd |] env Unix.stdin fd fd
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

let resolve_root file dir root = match root with
    | None   -> dir / file
    | Some r -> r / dir / file

let run_cram_tests ?syntax t ?root ppf temp_file pad tests =
  Block.pp_header ?syntax ppf t;
  let pad =
    match syntax with
    | Some Cram -> pad + 2
    | _ -> pad
  in
  List.iter (fun test ->
      let root = root_dir ?root t in
      let blacklist = Block.unset_variables t in
      let n = run_test ?root blacklist temp_file test in
      let lines = Mdx.Util.File.read_lines temp_file in
      let output =
        let output = List.map output_from_line lines in
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
    let lines = String.cuts ~sep:"\n" s in
    List.append lines acc
  in
  List.fold_left aux [] (List.rev lines)

let run_toplevel_tests ?syntax ?root c ppf tests t =
  Block.pp_header ?syntax ppf t;
  List.iter (fun test ->
      let lines = lines (eval_test ?root t c test) in
      let lines = split_lines lines in
      let output =
        let output = List.map output_from_line lines in
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
  Block.pp_footer ?syntax ppf ()

type file = { first: Mdx_top.Part.file; current: Mdx_top.Part.file }

let files: (string, file) Hashtbl.t = Hashtbl.create 8

let has_changed ~force_output { first; current } =
  let contents = Mdx_top.Part.contents current in
  if contents = Mdx_top.Part.contents first && force_output = false
  then None
  else Some contents

let read_parts file =
  try Hashtbl.find files file
  with Not_found ->
    match Mdx_top.Part.read file with
    | exception Sys_error msg -> failwith msg
    | parts ->
      let f = { first=parts; current=parts} in
      Hashtbl.add files file f;
      f

let read_part file part =
  let parts = read_parts file in
  match Mdx_top.Part.find parts.current ~part with
  | None       ->
    Fmt.failwith "Cannot find part %S in %s"
      (match part with None -> "" | Some p -> p)
      file
  | Some lines ->
    let contents = String.concat ~sep:"\n" lines in
    String.trim contents

let write_parts ~force_output file parts =
  let output_file = file ^ ".corrected" in
  match has_changed ~force_output parts with
  | None   -> if Sys.file_exists output_file then Sys.remove output_file
  | Some c ->
    let oc = open_out output_file in
    output_string oc c;
    flush oc;
    close_out oc

let update_block_content ppf t content =
  Block.pp_header ppf t;
  Output.pp ppf (`Output content);
  Block.pp_footer ppf ()

let update_file_or_block ?root ppf md_file ml_file block =
  let root = root_dir ?root block in
  let dir = Filename.dirname md_file in
  let ml_file = resolve_root ml_file dir root in
  update_block_content ppf block (read_part ml_file (Block.part block))

exception Test_block_failure of Block.t * string

let run_exn (`Setup ()) (`Non_deterministic non_deterministic)
    (`Not_verbose not_verbose) (`Syntax syntax) (`Silent silent)
    (`Verbose_findlib verbose_findlib) (`Prelude prelude)
    (`Prelude_str prelude_str) (`File file) (`Section section) (`Root root)
    (`Force_output force_output) (`Output output) =
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
          let env, f = Mdx.Prelude.env_and_file p in
          let eval () = eval_raw Block.empty ?root c ~line:0 [f] in
          match env with
          | None   -> eval ()
          | Some e -> Mdx_top.in_env e eval
        ) fs
    | fs, [] ->
      List.iter (fun p ->
          let env, f = Mdx.Prelude.env_and_file p in
          let eval () = eval_raw Block.empty ?root c ~line:0 (Mdx.Util.File.read_lines f) in
          match env with
          | None   -> eval ()
          | Some e -> Mdx_top.in_env e eval
        ) fs
    | _ -> Fmt.failwith "only one of --prelude or --prelude-str shoud be used"
  in

  let test_block ~ppf ~temp_file t =
    let active =
      active t && Block.version_enabled t && (not (Block.skip t))
    in
    let print_block () = Block.pp ?syntax ppf t in
    match active,
          non_deterministic,
          Block.file t,
          Block.mode t,
          Block.value t
    with
    (* Print errors *)
    | _, _, _, _, Error _
    (* The command is not active, skip it. *)
    | false, _, _, _, _
    (* the command is active but non-deterministic so skip everything *)
    | true, false, _, `Non_det `Command, _ -> print_block ()
    (* the command is active but it's output is
       non-deterministic; run it but keep the old output. *)
    | true, false, _, `Non_det `Output, Cram { tests; _ } ->
      print_block ();
      let blacklist = Block.unset_variables t in
      List.iter (fun t ->
          let _: int = run_test ?root blacklist temp_file t in ()
        ) tests
    | true, false, _, `Non_det `Output, Toplevel tests ->
      assert (syntax <> Some Cram);
      print_block ();
      List.iter (fun test ->
          match eval_test t ?root c test with
          | Ok _    -> ()
          | Error e ->
            let output = List.map (fun l -> `Output l) e in
            if Output.equal test.output output then ()
            else err_eval ~cmd:test.command e
        ) tests
    | true, _, Some ext_file, _, kind ->
        (match kind with
        | OCaml | Toplevel _ ->
          assert (syntax <> Some Cram);
          update_file_or_block ?root ppf file ext_file t
        | _ when Util.Option.is_some (Block.part t) ->
          Fmt.failwith
            "Parts are not supported for non-OCaml code blocks."
        | Cram _ | Raw ->
          let new_content = (read_part ext_file (Block.part t)) in
          update_block_content ppf t new_content
        | _ -> print_block ())
    | true, _, None, _, kind ->
      (match kind with
      | OCaml ->
        assert (syntax <> Some Cram);
        eval_raw t ?root c ~line:t.line t.contents;
        Block.pp ?syntax ppf t
      | Cram { tests; pad } ->
        run_cram_tests ?syntax t ?root ppf temp_file pad tests
      | Toplevel tests ->
        assert (syntax <> Some Cram);
        run_toplevel_tests ?syntax ?root c ppf tests t
      | Raw | _ -> print_block ())
  in
  let gen_corrected file_contents items =
    let temp_file = Filename.temp_file "ocaml-mdx" ".output" in
    at_exit (fun () -> Sys.remove temp_file);
    let buf = Buffer.create (String.length file_contents + 1024) in
    let ppf = Format.formatter_of_buffer buf in
    List.iter (function
        | Section _
        | Text _ as t -> Mdx.pp_line ?syntax ppf t
        | Block t ->
          List.iter (fun (k, v) -> Unix.putenv k v) (Block.set_variables t);
          try
            Mdx_top.in_env (Block.environment t)
              (fun () -> test_block ~ppf ~temp_file t)
          with Failure msg ->
            raise (Test_block_failure (t, msg))
      ) items;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  (match (output : Cli.output option) with
   | Some Stdout -> Mdx.run_to_stdout ?syntax ~f:gen_corrected file
   | Some (File outfile) ->
     Mdx.run_to_file ?syntax ~outfile ~f:gen_corrected file
   | None ->
     Mdx.run ?syntax ~force_output ~f:gen_corrected file);
  Hashtbl.iter (write_parts ~force_output) files;
  0

let report_error_in_block block msg =
  let kind =
    match block.Block.value with
    | Raw | Error _ -> ""
    | OCaml -> "OCaml "
    | Cram _ -> "cram "
    | Toplevel _ -> "toplevel "
  in
  Fmt.epr "Error in the %scode block in %s at line %d:@]\n%s\n"
    kind block.file block.line msg

let run setup non_deterministic not_verbose syntax silent verbose_findlib
    prelude prelude_str file section root force_output output : int =
    try
      Printexc.record_backtrace false;
    run_exn setup non_deterministic not_verbose syntax silent verbose_findlib
      prelude prelude_str file section root force_output output
    with
    | Failure f ->
      prerr_endline f;
      1
    | Test_block_failure (block, msg) ->
      report_error_in_block block msg;
      1

(**** Cmdliner ****)

open Cmdliner

let cmd =
  let exits = Term.default_exits in
  let man = [] in
  let doc = "Test markdown files." in
  Term.(pure run
        $ Cli.setup $ Cli.non_deterministic $ Cli.not_verbose $ Cli.syntax
        $ Cli.silent $ Cli.verbose_findlib $ Cli.prelude $ Cli.prelude_str
        $ Cli.file $ Cli.section $ Cli.root $ Cli.force_output $ Cli.output),
  Term.info "ocaml-mdx-test" ~version:"%%VERSION%%" ~doc ~exits ~man

let main () = Term.(exit_status @@ eval cmd)

let () = main ()
