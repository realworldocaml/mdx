val calculate :
  build_only: bool ->
  local_packages: (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  OpamStateTypes.unlocked OpamStateTypes.switch_state ->
  (OpamPackage.t list, [> `Msg of string]) result
(** Calculates a solution for the provided local packages and their opam files
    containing their regular and test dependencies using the provided opam switch
    state. Uses [Opam_0install].
    If [build_only] then no test dependencies are taken into account. *)
