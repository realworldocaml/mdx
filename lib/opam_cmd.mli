val calculate_opam :
  build_only:bool ->
  local_paths:(string option * Fpath.t) Import.String.Map.t ->
  local_packages:Types.Opam.package list ->
  OpamStateTypes.unlocked OpamStateTypes.switch_state ->
  (Opam.Package_summary.t list, [> `Msg of string ]) result

val report_packages_stats : Opam.Package_summary.t list -> unit
