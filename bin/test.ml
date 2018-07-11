let (/) x y = match x with
  | "." -> y
  | _   -> Filename.concat x y

let run () _ _ _ =
  let base = Filename.basename Sys.argv.(0) in
  let dir = Filename.dirname Sys.argv.(0) in
  let cmd = match base with
    | "main.exe" -> dir / "test" / "main.exe"
    | x -> dir / x ^ "-test"
  in
  Sys.argv.(0) <- cmd;
  Unix.execvp cmd Sys.argv

open Cmdliner

let cmd =
  let doc = "Test markdown files." in
  Term.(pure run $ Cli.setup $ Cli.non_deterministic $ Cli.file $ Cli.section),
  Term.info "test" ~doc
