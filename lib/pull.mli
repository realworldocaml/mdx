val duniverse :
  repo:Fpath.t ->
  global_state:OpamStateTypes.unlocked OpamStateTypes.global_state ->
  Duniverse.resolved Duniverse.Deps.Source.t list ->
  (unit, [> Rresult.R.msg ]) result
(** Pulls resolved source dependencies into [Config.vendor_dir] using the provided [cache].
    If [pull_mode] is [Duniverse.Config.Submodules], the cloned dependencies will be added as
    git submodules to the [repo]. *)
