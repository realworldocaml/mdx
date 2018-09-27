module Chunk :
  sig
    type kind = OCaml | Raw
    type response = kind * string
    type t = { ocaml_code : string; toplevel_responses : response list; }
    val v : ocaml_code:string -> toplevel_responses:response list -> t
    val code : t -> string
    val warnings : t -> string
    val responses : t -> response list
    val stdout : t -> string
    val evaluated : t -> bool
  end

module Part :
  sig
    type t = { name : string; chunks : Chunk.t list; }
    val v : name:string -> chunks:Chunk.t list -> t
    val name : t -> string
    val chunks : t -> Chunk.t list
  end

module Document :
  sig
    type t = { parts : Part.t list; matched : bool; }
    val v : parts:Part.t list -> matched:bool -> t
    val parts : t -> Part.t list
    val matched : t -> bool
  end

module Lexbuf :
  sig
    type t = { contents : string; lexbuf : Lexing.lexbuf; }
    val toplevel_fname : string
    val shift_toplevel_position :
      start:Lexing.position -> Lexing.position -> Lexing.position
    val shift_toplevel_location :
      start:Lexing.position -> Location.t -> Location.t
    val initial_pos : Lexing.position
    val semisemi_action : int
    val v : fname:string -> string -> t
    val of_file : string -> t
    val shift_location_error :
      Lexing.position -> Location.error -> Location.error
    val position_mapper : Lexing.position -> Ast_mapper.mapper
  end

module Phrase :
  sig
    type t = {
      startpos : Lexing.position;
      endpos : Lexing.position;
      parsed : (Parsetree.toplevel_phrase, exn) result;
    }
    val result : t -> (Parsetree.toplevel_phrase, exn) result
    val start : t -> Lexing.position
    val read : Lexbuf.t -> t option
    type 'a kind =
        Code of 'a
      | Expect of { location : Location.t; responses : Chunk.response list;
          nondeterministic : bool;
        }
      | Part of { location : Location.t; name : string; }
    type v = (t * Chunk.response list kind) list
    exception Cannot_parse_payload of Location.t
    val string_of_location : Location.t -> string
    val payload_constants :
      Location.t ->
      Parsetree.payload ->
      (Location.t * Longident.t Asttypes.loc option * Parsetree.constant)
      list
    val payload_strings :
      Location.t -> Parsetree.payload -> (Chunk.kind * string) list
    val attr_is : 'a Asttypes.loc -> 'a -> bool
    val kind : t -> unit kind
    val skip_whitespace : string -> ?stop:int -> int -> int
    val contents : Lexbuf.t -> ?start:int -> ?stop:int -> t -> string
    val whitespace : Lexbuf.t -> t -> (t * 'a) list -> string
    val document :
      Lexbuf.t ->
      matched:bool -> (t * Chunk.response list kind) list -> Document.t
    val is_findlib_directive : t -> bool
    val dry_exec :
      ('a * unit kind) list -> ('a * Chunk.response list kind) list
    val read_all : Lexbuf.t -> (t * Chunk.response list kind) list
    val find_delim : string -> string
    val is_whitespace : char -> bool
    val is_all_whitespace : string -> bool
    val is_ellision_line : string -> bool
    val string_subequal : string -> string -> int -> bool
    val match_outcome_chunk : 'a * string -> 'a * string -> bool
    val match_outcome : ('a * string) list -> ('a * string) list -> bool
    val validate :
      run_nondeterministic:bool ->
      ('a * Chunk.response list kind) list ->
      bool * ('a * Chunk.response list kind) list
    val output :
      out_channel ->
      Lexbuf.t -> (t * (Chunk.kind * string) list kind) list -> unit
  end
