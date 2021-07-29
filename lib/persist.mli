val load_sexp :
  string -> (Sexplib.Sexp.t -> 'a) -> Fpath.t -> ('a, [> Rresult.R.msg ]) result

val save_sexp :
  string ->
  ('a -> Sexplib.Sexp.t) ->
  Fpath.t ->
  'a ->
  (unit, [> Rresult.R.msg ]) result

val write_lines_hum :
  Fpath.t -> string list -> (unit, [> Rresult.R.msg ]) result
(** Same as [Bos.OS.File.write_lines] but adds a newline at the end of the file. *)
