module type CONTEXT = sig
  type t

  type rejection
  (** A reason why a package can't be used as input to the solver. e.g. it is
      for a different platform, or conflicts with a user-provided constraint. *)

  val pp_rejection : rejection Fmt.t

  val load : t -> OpamPackage.t -> OpamFile.OPAM.t
  (** [load t pkg] loads the opam metadata for [pkg]. *)

  val candidates : t -> OpamPackage.Name.t -> (OpamPackage.Version.t * rejection option) list
  (** [candidates t name] is the list of available versions of [name], in order
      of decreasing preference. If the user or environment provides additional
      constraints that mean a version should be rejected, include that here too. Rejects
      are only used for generating diagnostics reports. *)

  val user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
  (** [user_restrictions t pkg] is the user's constraint on [pkg], if any. This is just
      used for diagnostics; you still have to filter them out yourself in [candidates]. *)

  val filter_deps : t -> OpamPackage.t -> OpamTypes.filtered_formula -> OpamTypes.formula
  (** [filter_deps t pkg f] is used to pre-process depends and conflicts.
      [pkg] is the package which has the dependency [f].
      For example, you can use this to filter out dependencies that are only needed on Windows
      if the platform is Linux. *)
end
