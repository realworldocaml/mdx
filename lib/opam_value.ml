open Import

let default_pos = ("None", 0, 0)

let pos_errorf ~pos fmt =
  let file, line, char = pos in
  Format.ksprintf
    (fun msg -> Error (`Msg msg))
    ("Error in %s, line %d, col %d: " ^^ fmt)
    file line char

let rec from_sexp ?(pos = default_pos) sexp : OpamTypes.value =
  match (sexp : Sexplib.Sexp.t) with
  | Atom s -> String (pos, s)
  | List l -> List (pos, List.map ~f:(from_sexp ~pos) l)

let rec to_sexp_strict value =
  let open Result.O in
  match (value : OpamTypes.value) with
  | String (_, s) -> Ok (Sexplib0.Sexp.Atom s)
  | List (_, l) -> Result.List.all (List.map ~f:to_sexp_strict l) >>| fun l -> Sexplib0.Sexp.List l
  | Bool (pos, _)
  | Int (pos, _)
  | Relop (pos, _, _, _)
  | Prefix_relop (pos, _, _)
  | Logop (pos, _, _, _)
  | Pfxop (pos, _, _)
  | Ident (pos, _)
  | Group (pos, _)
  | Option (pos, _, _)
  | Env_binding (pos, _, _, _) ->
      pos_errorf ~pos "Expected a sexp compatible opam value"
