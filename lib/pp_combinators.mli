module Ocaml : sig
  (** Combinators to pretty print OCaml values *)

  val string : string Fmt.t
  (** [string fmt s] pretty prints [s], with quotes *)

  val option : ?brackets:bool -> 'a Fmt.t -> 'a option Fmt.t
  (** [ocaml_option ~brackets pp_a fmt a_option] pretty prints an ocaml option as
      ["None"], ["Some %a"] or ["Some (%a)"] if [brackets] is true. It uses [pp_a] to print
      the value wrapped in the option.
      [brackets] default to false. *)

  val list : 'a Fmt.t -> 'a list Fmt.t
  (** [ocaml_list pp_a fmt l] pretty prints l as an ocaml looking list, using [pp_a] to format
      individual elements. *)
end
