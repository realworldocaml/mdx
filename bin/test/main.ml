open Astring
open Mdx

let src = Logs.Src.create "cram.test"
module Log = (val Logs.src_log src : Logs.LOG)

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

let run_test temp_file t =
  let cmd = Cram.command_line t in
  Log.info (fun l -> l "exec: %S" cmd);
  let fd = Unix.openfile temp_file [O_WRONLY; O_TRUNC] 0 in
  let pid = Unix.create_process "sh" [| "sh"; "-c"; cmd |] Unix.stdin fd fd in
  Unix.close fd;
  match snd (Unix.waitpid [] pid) with
  | WEXITED n -> n
  | _ -> 255

let run_cram_tests ppf temp_file pad tests t =
  Block.pp_header ppf t;
  List.iter (fun test ->
      let n = run_test temp_file test in
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

let init_toplevel () =
  Toploop.set_paths ();
  Compmisc.init_path true;
  Toploop.toplevel_env := Compmisc.initial_env ();
  Sys.interactive := false

let run_toplevel_tests ppf pad tests t =
  Block.pp_header ppf t;
  List.iter (fun test ->
      let lines = Mdx_top.run (Toplevel.command test) in
      let output =
        let output = List.map (fun x -> `Output x) lines in
        if Output.equal output test.output then test.output
        else output
      in
      Toplevel.pp_command ~pad ppf test;
      List.iter (function
          | `Ellipsis    -> Output.pp ~pad ppf `Ellipsis
          | `Output line ->
            let line = ansi_color_strip line in
            Output.pp ~pad ppf (`Output line)
        ) output;
    ) tests;
  Block.pp_footer ppf ()


let run () non_deterministic expect_test section =
  let section = match section with
    | None   -> None
    | Some p -> Some (Re.Perl.compile_pat p)
  in
  let active b = match section, Block.section b with
    | None   , _      -> true
    | Some re, None   -> Re.execp re ""
    | Some re, Some s -> Re.execp re (snd s)
  in
  init_toplevel ();
  Mdx.run expect_test ~f:(fun file_contents items ->
      let temp_file = Filename.temp_file "mdx" ".output" in
      at_exit (fun () -> Sys.remove temp_file);
      let buf = Buffer.create (String.length file_contents + 1024) in
      let ppf = Format.formatter_of_buffer buf in
      List.iter (function
          | Section _
          | Text _ as t -> Mdx.pp_line ppf t
          | Block t ->
            match active t, non_deterministic, Block.mode t, Block.value t with
            (* The command is not active, skip it. *)
            | false, _, _, _ -> Block.pp ppf t
            (* the command is active but non-deterministic so skip everything *)
            | true, false, `Non_det `Command, _ -> Block.pp ppf t
            (* the command is active but it''s output is
               non-deterministic; run it but keep the old output. *)
            | true, false, `Non_det `Output, Cram { tests; _ } ->
              Block.pp ppf t;
              List.iter (fun t -> let _ = run_test temp_file t in ()) tests
            (* Skip raw blocks. *)
            | true, _, _, Raw -> Block.pp ppf t
            (* Cram tests. *)
            | true, _, _, Cram { tests; pad } ->
              run_cram_tests ppf temp_file pad tests t
            (* Top-level tests. *)
            | true, _, _, Toplevel { tests; pad } ->
              run_toplevel_tests ppf pad tests t
        ) items;
      Format.pp_print_flush ppf ();
      Buffer.contents buf);
  0

open Cmdliner

let cmd =
  let exits = Term.default_exits in
  let man = [] in
  let doc = "Test markdown files." in
  Term.(pure run $ Cli.setup $ Cli.non_deterministic $ Cli.file $ Cli.section),
  Term.info "mdx-test" ~version:"%%VERSION%%" ~doc ~exits ~man

let main () = Term.(exit_status @@ eval cmd)

let () = main ()
