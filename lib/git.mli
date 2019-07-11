val parse_ls_remote_line : string -> (string * string, Rresult.R.msg) result
(** Parse the given git ls-remote output line and return the pair
    [(commit_hash, full_qualified_ref)]. *)

module Ref : sig
  type t = string

  val equal : t -> t -> bool

  val pp : t Fmt.t

  type resolved = { t : t; commit : string } [@@deriving sexp]

  val equal_resolved : resolved -> resolved -> bool

  val pp_resolved : resolved Fmt.t
end
