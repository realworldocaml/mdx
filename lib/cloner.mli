type cache

val get_cache : unit -> (cache, [> `Msg of string ]) result

val no_cache : cache

val clone_to :
  output_dir:Fpath.t ->
  remote:string ->
  ref:Git.Ref.t ->
  commit:string ->
  cache ->
  (bool, Rresult.R.msg) result
(** [clone_to ~output_dir ~remote ~ref ~commit cache] uses the cache system to clone a particular
    commit to the given directory.
    This directory is overwritten. *)
