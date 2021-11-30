type explicit_repos = string list

type switch = OpamStateTypes.unlocked OpamStateTypes.switch_state

type ('context, 'diagnostics) t

type switch_diagnostics

type explicit_repos_diagnostics

val local_opam_config_solver : (switch, switch_diagnostics) t

val explicit_repos_solver : (explicit_repos, explicit_repos_diagnostics) t

val calculate :
  build_only:bool ->
  allow_jbuilder:bool ->
  local_opam_files:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  target_packages:OpamPackage.Name.Set.t ->
  pin_depends:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
  ?ocaml_version:string ->
  ('context, 'diagnostics) t ->
  'context ->
  ( Opam.Package_summary.t list,
    [> `Diagnostics of 'diagnostics | `Msg of string ] )
  result
(** Calculates a solution for the provided local packages and their opam files
    containing their regular and test dependencies using the provided opam switch
    state. Uses [Opam_0install].
    If [build_only] then no test dependencies are taken into account. If [ocaml_version]
    is provided, the solution will contain that concrete version of ocaml. *)

val diagnostics_message :
  verbose:bool -> (_, 'diagnostics) t -> 'diagnostics -> [> `Msg of string ]

val not_buildable_with_dune :
  (_, 'diagnostics) t -> 'diagnostics -> OpamPackage.Name.t list
