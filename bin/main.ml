open Cmdliner

let cmds = [Test.cmd; Pp.cmd ]
let main () = `Help (`Pager, None)

let main =
  let doc = "Execute markdown files." in
  let exits = Term.default_exits in
  let man = [] in
  Term.(ret (const main $ Cli.setup)),
  Term.info "mdx" ~version:"%%VERSION%%" ~doc ~exits ~man

let main () = Term.(exit_status @@ eval_choice main cmds)

let () = main ()
