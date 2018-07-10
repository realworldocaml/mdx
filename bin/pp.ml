let src = Logs.Src.create "cram.pp"
module Log = (val Logs.src_log src : Logs.LOG)

let run () file section =
  let t = Mdx.parse_file file in
  let t = match section with
    | None   -> t
    | Some s ->
      let re = Re.Perl.compile_pat s in
      match Mdx.filter_section re t with
      | None   -> []
      | Some t -> t
  in
  match t with
  | [] -> 1
  | _  ->
    List.iter (function
        | Mdx.Section _ | Text _ -> ()
        | Block b ->
          let b = Mdx.Block.eval b in
          Log.debug (fun l -> l "pp: %a" Mdx.Block.dump b);
          let pp_lines = Fmt.(list ~sep:(unit "\n") string) in
          let contents = Mdx.Block.executable_contents b in
          Fmt.pr "#%d %S\n%a\n" b.line file pp_lines contents
      ) t;
    0

open Cmdliner

let cmd =
  let doc = "Pre-process markdown files to produce OCaml code." in
  let exits = Term.default_exits in
  Term.(pure run $ Cli.setup $ Cli.file $ Cli.section),
  Term.info "pp" ~doc ~exits
