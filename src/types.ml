open Sexplib.Conv

type t = {
  name: int;
} [@@deriving sexp]
