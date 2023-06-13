type t = Markdown | Latex | Cram | Mli | Mld

let pp fs = function
  | Markdown -> Fmt.string fs "markdown"
  | Latex -> Fmt.string fs "latex"
  | Cram -> Fmt.string fs "cram"
  | Mli -> Fmt.string fs "mli"
  | Mld -> Fmt.string fs "mld"

let equal x y = x = y

let infer ~file =
  match Filename.extension file with
  | ".t" -> Some Cram
  | ".md" -> Some Markdown
  | ".tex" -> Some Latex
  | ".mli" -> Some Mli
  | ".mld" -> Some Mld
  | _ -> None

let of_string = function
  | "markdown" | "normal" -> Some Markdown
  | "latex" -> Some Latex
  | "cram" -> Some Cram
  | "mli" -> Some Mli
  | "mld" -> Some Mld
  | _ -> None
