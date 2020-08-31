val cmd : unit Cmdliner.Term.t * Cmdliner.Term.info

val run :
           [< `Repo of Fpath.t ] ->
           [< `Opam_repo of Uri_sexp.t ] ->
           [< `Pull_mode of Duniverse_lib.Duniverse.Config.pull_mode ] ->
           unit -> (unit, Rresult.R.msg) result