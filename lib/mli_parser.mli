module Code_block : sig
  type t =
    { location : Odoc_model.Location_.span
    ; contents : string
    }
end

(** Find the substring given by two line:col positions. *)
val slice
  :  string
  -> start:Odoc_model.Location_.point
  -> end_:Odoc_model.Location_.point
  -> string

(** Parse an mli file as a string and return a list of the code blocks that appear inside
    its docstrings. *)
val docstring_code_blocks : string -> Code_block.t list
