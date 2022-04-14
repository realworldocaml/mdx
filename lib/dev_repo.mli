open Import

type t

val from_string : string -> t
val to_string : t -> string

val repo_name : t -> string
(** Returns the name of the repo given the dev-repo.
    E.g. [repo_name (from_string "https://github.com/ocamllabs/opam-monorepo.git")]
    returns ["opam-monorepo"]. *)

module Map : Map.S with type key = t
