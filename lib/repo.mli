(** Utility functions to extract repository specific path and values *)

type t = Fpath.t

(** Returns the locally defined opam packages as a map from package names to
    to the corresponding .opam file path.
    Only considers packages defined at the repo's root. *)
val local_packages : t -> (Fpath.t Astring.String.Map.t, [> `Msg of string]) result
