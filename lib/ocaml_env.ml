type t = Default | User_defined of string

let mk = function None | Some "" -> Default | Some s -> User_defined s
let name = function Default -> "" | User_defined s -> s

let pp ppf = function
  | Default -> Fmt.string ppf "Default"
  | User_defined s -> Fmt.pf ppf "User_defined %S" s

type env = t

module Set = Set.Make (struct
  type t = env

  let compare = compare
end)
