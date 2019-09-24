val clone_to :
  output_dir:Fpath.t ->
  remote:string ->
  ref:Git.Ref.t ->
  commit:string ->
  unit ->
  (bool, Rresult.R.msg) result
(** [clone_to ~output_dir ~remote ~ref ~commit ()] uses the cache system to clone a particular
    commit to the given directory.
    This directory is overwritten. *)
