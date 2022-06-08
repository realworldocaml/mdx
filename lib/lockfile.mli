type t

module Depends : sig
  type dependency = { package : OpamPackage.t; vendored : bool }
  type t = dependency list
end

val create :
  source_config:Source_opam_config.t ->
  root_packages:OpamPackage.Name.Set.t ->
  dependency_entries:Opam.Dependency_entry.t list ->
  root_depexts:(OpamSysPkg.Set.t * OpamTypes.filter) list list ->
  duniverse:Duniverse.t ->
  unit ->
  t

val depends : t -> Depends.t
val to_duniverse : t -> (Duniverse.t, [ `Msg of string ]) result
val ocaml_version : t -> OpamPackage.Version.t option

val save :
  opam_monorepo_cwd:Fpath.t ->
  cli_args:string list ->
  file:Fpath.t ->
  t ->
  (unit, [ `Msg of string ]) result

val load :
  opam_monorepo_cwd:Fpath.t -> file:Fpath.t -> (t, [ `Msg of string ]) result

val depexts : t -> (OpamSysPkg.Set.t * OpamTypes.filter) list
