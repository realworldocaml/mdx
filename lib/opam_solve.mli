val calculate :
  build_only:bool ->
  allow_jbuilder:bool ->
  local_opam_files:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?ocaml_version:string ->
  OpamStateTypes.unlocked OpamStateTypes.switch_state ->
  (Opam.Package_summary.t list, [> `Msg of string ]) result
(** Calculates a solution for the provided local packages and their opam files
    containing their regular and test dependencies using the provided opam switch
    state. Uses [Opam_0install].
    If [build_only] then no test dependencies are taken into account. If [ocaml_version]
    is provided, the solution will contain that concrete version of ocaml. *)
