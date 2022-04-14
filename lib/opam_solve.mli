open Import

type explicit_repos = string list
type opam_env = OpamVariable.variable_contents String.Map.t
type switch = OpamStateTypes.unlocked OpamStateTypes.switch_state
type ('context, 'diagnostics) t
type switch_diagnostics
type explicit_repos_diagnostics

val local_opam_config_solver : (switch, switch_diagnostics) t

val explicit_repos_solver :
  (opam_env * explicit_repos, explicit_repos_diagnostics) t

val calculate :
  build_only:bool ->
  allow_jbuilder:bool ->
  require_cross_compile:bool ->
  local_opam_files:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  target_packages:OpamPackage.Name.Set.t ->
  opam_provided:OpamPackage.Name.Set.t ->
  pin_depends:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?ocaml_version:string ->
  ('context, 'diagnostics) t ->
  'context ->
  ( Opam.Dependency_entry.t list,
    [> `Diagnostics of 'diagnostics | `Msg of string ] )
  result
(** Calculates a solution for the provided local packages and their Opam files
    containing their regular and test dependencies using the provided opam switch
    state. Uses [Opam_0install].
    If [build_only] then no test dependencies are taken into account. If [ocaml_version]
    is provided, the solution will contain that concrete version of ocaml. *)

val diagnostics_message :
  verbose:bool -> (_, 'diagnostics) t -> 'diagnostics -> [> `Msg of string ]

val not_buildable_with_dune :
  (_, 'diagnostics) t -> 'diagnostics -> OpamPackage.Name.t list

val unavailable_versions_due_to_constraints :
  (_, 'diagnostics) t ->
  'diagnostics ->
  (OpamPackage.Name.t * OpamFormula.version_formula) list
