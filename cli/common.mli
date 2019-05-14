module Arg : sig
  val fpath : Fpath.t Cmdliner.Arg.converter

  val repo : Fpath.t Cmdliner.Term.t
  (** CLI option to specify the root directory of the project. Used to find root packages,
      duniverse files and directories. Defaults to the current directory. *)

  val branch : string Cmdliner.Term.t
  (** CLI option to specify the branch on which duniverse must operate in the project. *)

  val setup_logs : unit -> unit Cmdliner.Term.t
  (** Adds the common options -v and --version and sets up the logs before being passed as [()] to a
      command. *)
end
