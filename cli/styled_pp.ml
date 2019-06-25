let header = Fmt.(styled `Blue (const string "==> "))

let branch = Fmt.(styled `Cyan string)

let package = Fmt.(styled `Yellow Duniverse_lib.Types.Opam.pp_package)
