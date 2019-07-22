val git_get :
  output_dir:Fpath.t -> remote:string -> tag:string -> unit -> (unit, [> Rresult.R.msg]) result
(** [git_get ~output_dir ~remote ~tag ()]] fetch a git project from the given remote at the given tag and place it in the output_dir.
    It uses a cache mechanism to locally fetch the (remote, tag) pair when it's already available. The cache assumes that the given tag
    won't be updated on the remote. output_dir and it's content will be removed if it exists.
    Returns true if the result was in the cache.
*)
