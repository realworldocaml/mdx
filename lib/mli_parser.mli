val parse_mli : string -> (Document.line list, [ `Msg of string ] list) result
(** Slice an mli file into its [Text] and [Block] parts. *)

val parse_mld :
  ?filename:string ->
  string ->
  (Document.line list, [ `Msg of string ] list) result
(** Slice an mld file into its [Text] and [Block] parts. *)
