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
          if Mdx.Block.is_raw_ocaml b then
            Fmt.pr "#%d %S\n%a" b.line file Mdx.Block.pp_contents b
      ) t;
    0

open Cmdliner

let cmd =
  let doc = "Pre-process markdown files to produce OCaml code." in
  let exits = Term.default_exits in
  Term.(pure run $ Cli.setup $ Cli.file $ Cli.section),
  Term.info "pp" ~doc ~exits
