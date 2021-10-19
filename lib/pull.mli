val duniverse :
  full:bool ->
  repo:Fpath.t ->
  global_state:OpamStateTypes.unlocked OpamStateTypes.global_state ->
  trim_clone:bool ->
  Duniverse.t ->
  (unit, [> Rresult.R.msg ]) result
(** [duniverse ~full ~repo ~global_state duniverse]
    pulls duniverse repositories into the [Config.vendor_dir] of the given [repo].
    If [full] is [true] then the vendor_dir is entirely deleted before pulling, otherwise
    only the subfolder corresponding to the repos in [duniverse] are. *)
