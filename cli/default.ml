let run () = `Help (`Pager, None)

let term = Cmdliner.Term.(ret (const run $ const ()))
