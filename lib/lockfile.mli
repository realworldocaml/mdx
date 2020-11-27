type t

val create :
  root_packages:string list ->
  package_summaries:Opam.Package_summary.t list ->
  root_depexts:(OpamSysPkg.Set.t * OpamTypes.filter) list list ->
  duniverse:Duniverse.t ->
  unit ->
  t

val to_duniverse : t -> (Duniverse.t, [ `Msg of string ]) result

val save : file:Fpath.t -> t -> (unit, [ `Msg of string ]) result

val load : file:Fpath.t -> (t, [ `Msg of string ]) result
