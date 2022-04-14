module Raw : sig
  val as_sexps : Fpath.t -> (Sexplib0.Sexp.t list, [> `Msg of string ]) result
  (** Parses a dune file as a list of S-expressions. *)

  val comment : string -> string

  val vendored_dirs : string -> string
  (** [vendored_dirs glob] returns a stanza marking directories matching [glob] as vendored *)

  val duniverse_dune_content : string list
  (** The content of the duniverse/dune file as a list of lines *)
end

module Lang : sig
  type version = int * int

  val compare_version : version -> version -> int
  val pp_version : version Fmt.t

  val duniverse_minimum_version : version
  (** The minimum dune lang version required by duniverse *)

  val from_content : string -> (version, [> `Msg of string ]) result
  (** Extract the lang version from the content of the entire dune-project *)

  val update : version:version -> string -> string
  (** Update the content of the entire dune-project, setting the lang version
      to [version].
      Return the string unmodified if there was previously no lang stanza. *)
end

module Project : sig
  val name : Sexplib0.Sexp.t list -> (string, [> `Msg of string ]) result
  (** Returns the dune-project's name given the content of the file as a list of S-expressions,
      if any. *)
end
