module Logs : sig
  val app : ?src:Logs.src -> 'a Logs.log
  (** Formats the arguments and logs the resulting message with app level, preceded by a sexy looking
      ["==> "] blue header. *)
end

module Arg : sig
  val fpath : Fpath.t Cmdliner.Arg.converter

  val repo : Fpath.t Cmdliner.Term.t
  (** CLI option to specify the root directory of the project. Used to find root packages,
      duniverse files and directories. Defaults to the current directory. *)

  val setup_logs : unit -> unit Cmdliner.Term.t
  (** Adds the common options -v and --version and sets up the logs before being passed as [()] to a
      command. *)
end
