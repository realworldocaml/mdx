module Raw : sig
  val comment : string -> string

  val vendored_dirs : string -> string
  (** [vendored_dirs glob] returns a stanza marking directories matching [glob] as vendored *)

  val duniverse_dune_content : string list
  (** The content of the duniverse/dune file as a list of lines *)
end
