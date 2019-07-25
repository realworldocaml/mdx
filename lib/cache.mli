val clone_to :
  output_dir:Fpath.t ->
  remote:string ->
  ref:Git.Ref.t ->
  commit:string ->
  unit ->
  (bool, Rresult.R.msg) result
(** [clone_to ~output_dir ~remote ~ref ~commit ()] uses the cache system to clone a particular commit to the given directory.
  This directory is overwritten. *)

val update : remote:string -> ref:Git.Ref.t -> unit -> (unit, Rresult.R.msg) result
(** [update ~remote ~ref ()] updates a cached branch using the remote. *)

val resolve : remote:string -> ref:Git.Ref.t -> unit -> (Git.Ref.resolved, Rresult.R.msg) result
(** [resolve ~remote ~ref ()] locally resolve the given ref to a commit hash. *)
