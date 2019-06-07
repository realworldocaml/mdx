module Dev_repo : sig
  type vcs = Git | Other of string

  val equal_vcs : vcs -> vcs -> bool

  val pp_vcs : vcs Fmt.t

  type t = { vcs : vcs option; uri : Uri.t }

  val equal : t -> t -> bool

  val pp : t Fmt.t

  val from_string : string -> t
end
