val parse_mli : string -> (Document.line list, [ `Msg of string ]) Result.result
(** Slice an mli file into its [Text] and [Block] parts. *)

val parse_mld :
  fname:string ->
  text:string ->
  (Document.line list, [ `Msg of string ]) Result.result
