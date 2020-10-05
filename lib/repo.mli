(** Utility functions to extract repository specific path and values *)

type t = Fpath.t

(** Returns the locally defined opam packages as a map from package names to
    to the corresponding .opam file path.
    Only considers packages defined at the repo's root unless [recurse] is [true]. *)
val local_packages :
  recurse: bool ->
  t ->
  (Fpath.t Astring.String.Map.t, [> `Msg of string]) result

(** Returns the path to the dune-project file. *)
val dune_project : t -> Fpath.t

(** Returns the name of the project, as set in the dune-project. *)
val project_name : t -> (string, [> `Msg of string]) result

(** Returns the path to the duniverse lock file for the given repo.
    If the repo contains a single package, then it's the ["<package_name>.opam.locked"]
    file at the root of the repo.
    If it contains multiple packages, then it's the ["<project_name>.opam.locked"] file
    at the root of the repo.
    One can provide [local_packages] if they were already computed. *)
val duniverse_file :
  ?local_packages: Types.Opam.package list ->
  t ->
  (Fpath.t, [> `Msg of string]) result
