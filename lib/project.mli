(** Utility functions to extract project specific path and values *)

type t = Fpath.t
(** The type of projects.
    
    What we consider a project here is the root of a dune project/workspace *)

val local_packages :
  recurse:bool ->
  t ->
  ((OpamPackage.Name.t * Fpath.t) list, [> Rresult.R.msg ]) result
(** Returns the locally defined opam packages as an association list from package names to
    to the corresponding .opam file path.
    Only considers packages defined at the repo's root unless [recurse] is [true].  *)

val all_local_packages :
  t -> ((OpamPackage.Name.t * Fpath.t) list, [> Rresult.R.msg ]) result
(** [all_local_packages t] is [local_packages ~recurse:true t].  *)

val dune_project : t -> Fpath.t
(** Returns the path to the dune-project file. *)

val name : t -> (string, [> `Msg of string ]) result
(** Returns the name of the project, as set in the dune-project. *)

val lockfile :
  ?local_packages:OpamPackage.Name.t list ->
  t ->
  (Fpath.t, [> `Msg of string ]) result
(** Returns the path to the opam-monorepo lockfile for the given project.
    If the repo contains a single package, then it's the ["<package_name>.opam.locked"]
    file at the root of the project.
    If it contains multiple packages, then it's the ["<project_name>.opam.locked"] file
    at the root of the project.
    One can provide [local_packages] if they were already computed or if only a subset
    of the local packages must be taken into account. *)

val local_lockfiles : t -> (Fpath.t list, Rresult.R.msg) result
(** Returns all the lockfiles located at the root of the project i.e. all
    .opam.locked files. *)
