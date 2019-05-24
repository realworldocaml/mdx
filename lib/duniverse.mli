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

val load : file:Fpath.t -> (t, [> `Msg of string ]) result

val save : file:Fpath.t -> t -> (unit, [> `Msg of string ]) result
