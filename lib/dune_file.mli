open Stdune

module Raw : sig
  val comment : string -> string

  val vendored_dirs : string -> string
  (** [vendored_dirs glob] returns a stanza marking directories matching [glob] as vendored *)

  val duniverse_dune_content : string list
  (** The content of the duniverse/dune file as a list of lines *)

  val duniverse_minimum_lang : string
  (** The lang stanza setting the dune language to the minimum version required by duniverse *)
end

module Lang : sig
  type version = int * int

  val compare_version : version -> version -> Ordering.t

  val pp_version : version Fmt.t

  val duniverse_minimum_version : version
  (** The minimum dune lang version required by duniverse *)

  val parse_version : string -> (version, Rresult.R.msg) result

  val parse_stanza : string -> (version, Rresult.R.msg) result
  (** Parse the given lang stanza and return the dune language version *)

  val is_stanza : string -> bool
  (** Tells whether the given dune-project file line is a lang stanza *)
end
