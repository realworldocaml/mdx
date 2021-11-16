type diagnostics

val calculate :
  build_only:bool ->
  allow_jbuilder:bool ->
  local_opam_files:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  target_packages:OpamPackage.Name.Set.t ->
  pin_depends:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?ocaml_version:string ->
  OpamStateTypes.unlocked OpamStateTypes.switch_state ->
  ( Opam.Package_summary.t list,
    [> `Diagnostics of diagnostics | `Msg of string ] )
  result
(** Calculates a solution for the provided local packages and their opam files
    containing their regular and test dependencies using the provided opam switch
    state. Uses [Opam_0install].
    If [build_only] then no test dependencies are taken into account. If [ocaml_version]
    is provided, the solution will contain that concrete version of ocaml. *)

val diagnostics_message : verbose:bool -> diagnostics -> [> `Msg of string ]

val not_buildable_with_dune : diagnostics -> OpamPackage.Name.t list
