type t

val create :
  source_config:Source_opam_file.config ->
  root_packages:OpamPackage.Name.Set.t ->
  package_summaries:Opam.Package_summary.t list ->
  root_depexts:(OpamSysPkg.Set.t * OpamTypes.filter) list list ->
  duniverse:Duniverse.t ->
  unit ->
  t

val to_duniverse : t -> (Duniverse.t, [ `Msg of string ]) result

val save :
  opam_monorepo_cwd:string ->
  file:Fpath.t ->
  t ->
  (unit, [ `Msg of string ]) result

val load :
  opam_monorepo_cwd:string -> file:Fpath.t -> (t, [ `Msg of string ]) result

val depexts : t -> (OpamSysPkg.Set.t * OpamTypes.filter) list
