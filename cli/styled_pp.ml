let header = Fmt.(styled `Blue (const string "==> "))

let question_header = Fmt.(styled `Magenta (const string "??? "))

let branch = Fmt.(styled `Cyan string)

let package = Fmt.(styled `Yellow Duniverse_lib.Types.Opam.pp_package)

let path = Fmt.(styled `Cyan Fpath.pp)

let good pp = Fmt.(styled `Green pp)

let bad pp = Fmt.(styled `Red pp)
