type token =
  [ `Block of Block.Raw.t | `Section of int * string | `Text of string ]

val latex_token : Lexing.lexbuf -> (token list, [ `Msg of string ]) result
