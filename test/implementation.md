# Some Implementation

```ocaml
let src = Logs.Src.create "cram.test"
module Log = (val Logs.src_log src : Logs.LOG)

let (/) x y = match x with
  | "." -> y
  | _   -> Filename.concat x y

let run () _ _ _ _ _ _ _ _ _ _ =
  let base = Filename.basename Sys.argv.(0) in
  let dir = Filename.dirname Sys.argv.(0) in
  let cmd = match base with
    | "main.exe" -> dir / "test" / "main.exe"
    | x -> dir / x ^ "-test"
  in
  let argv = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) in
  argv.(0) <- cmd;
  Log.debug (fun l -> l "executing %a" Fmt.(Dump.array string) argv);
  Unix.execvp cmd argv

open Cmdliner

let cmd: int Term.t * Term.info =
  let doc = "Test markdown files." in
  Term.(pure run
        $ Cli.setup $ Cli.non_deterministic $ Cli.not_verbose
        $ Cli.silent $ Cli.verbose_findlib $ Cli.prelude $ Cli.prelude_str
        $ Cli.file $ Cli.section $ Cli.root $ Cli.direction),
  Term.info "test" ~doc
```