module Ls_remote : sig
  val ref_arg : string -> Bos.Cmd.t
  (** [ref_arg ref] returns the CLI arguments to pass to git ls-remote
      to find the commit pointed by [ref] even the target repository uses packed-refs. *)

  val commit_pointed_by :
    ref:string ->
    string list ->
    (string, [> `No_such_ref | `Multiple_such_refs | `Msg of string ]) result
  (** [commit_pointed_by ~ref ls_remote_output] parses the output from git ls-remote
      and returns the commit pointed by [ref] if it can be determined from it.
      It will work even if the repo uses packed-refs. *)

  (**/**)

  (* Exposed for test purposes only *)

  val parse_output_line : string -> (string * string, Rresult.R.msg) result
  (** Parse the given git ls-remote output line and return the pair
      [(commit_hash, fully_qualified_ref)]. *)

  (**/**)
end

module Ref : sig
  type t = string

  val equal : t -> t -> bool

  val pp : t Fmt.t

  type resolved = { t : t; commit : string } [@@deriving sexp]

  val equal_resolved : resolved -> resolved -> bool

  val pp_resolved : resolved Fmt.t
end
