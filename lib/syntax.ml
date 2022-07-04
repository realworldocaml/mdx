type t = Normal | Cram | Mli | Mld

let pp fs = function
  | Normal -> Fmt.string fs "normal"
  | Cram -> Fmt.string fs "cram"
  | Mli -> Fmt.string fs "mli"
  | Mld -> Fmt.string fs "mld"

let equal x y = x = y

let infer ~file =
  match Filename.extension file with
  | ".t" -> Some Cram
  | ".md" -> Some Normal
  | ".mli" -> Some Mli
  | ".mld" -> Some Mld
  | _ -> None

let of_string = function
  | "markdown" | "normal" -> Some Normal
  | "cram" -> Some Cram
  | "mli" -> Some Mli
  | "mld" -> Some Mld
  | _ -> None
