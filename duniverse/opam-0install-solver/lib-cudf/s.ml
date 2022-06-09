module type CONTEXT = sig
  type t

  type rejection
  (** A reason why a package can't be used as input to the solver. e.g. it is
      for a different platform, or conflicts with a user-provided constraint. *)

  val pp_rejection : Format.formatter -> rejection -> unit

  val candidates : t -> Cudf_types.pkgname -> (Cudf_types.version * (Cudf.package, rejection) result) list
  (** [candidates t name] is the list of available versions of [name], in order
      of decreasing preference. If the user or environment provides additional
      constraints that mean a version should be rejected, include that here too. Rejects
      are only used for generating diagnostics reports. *)

  val user_restrictions : t -> Cudf_types.pkgname -> (Cudf_types.relop * Cudf_types.version) list
  (** [user_restrictions t pkg] is the user's constraint on [pkg], if any. This is just
      used for diagnostics; you still have to filter them out yourself in [candidates]. *)
end
