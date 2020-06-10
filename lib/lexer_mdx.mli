type token = [ `Block of Block.t | `Section of int * string | `Text of string ]

val markdown_token : Lexing.lexbuf -> (token list, [ `Msg of string ]) Result.t

val cram_token : Lexing.lexbuf -> (token list, [ `Msg of string ]) Result.t
