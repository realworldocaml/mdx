open Sexplib.Conv

type t = {
  root_packages : Types.Opam.package list;
  excludes : Types.Opam.package list;
  pins : Types.Opam.pin list;
  packages : Types.Opam.entry list;
  repos : Types.Dune.repo list;
  remotes : string list; [@default []]
  branch : string [@default "master"]
}
[@@deriving sexp]

let load ~file = Persist.load_sexp "duniverse" t_of_sexp file

let save ~file t = Persist.save_sexp "duniverse" sexp_of_t file t
