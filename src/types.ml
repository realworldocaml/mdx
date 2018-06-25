open Sexplib.Conv

let pp_sexp fn ppf v = Fmt.pf ppf "%s" (Sexplib.Sexp.to_string_hum (fn v))

module Opam = struct
  type repo =
    [ `Github of string * string
    | `Duniverse_fork of string
      (** [name] which is [https://github.com/dune-universe/name] *)
    | `Unknown of string
    | `Virtual
    | `Error of string ]
  [@@deriving sexp]

  type opam =
    { name: string
    ; version: string
    ; dev_repo: repo
    ; tag: string option [@default None] [@sexp_drop_default]
    ; is_dune: bool [@default true] [@sexp_drop_default] }
  [@@deriving sexp]

  type t = {
    pkgs: opam list;
    roots: string list;
    excludes: string list; } [@@deriving sexp]

  let pp_repo = pp_sexp sexp_of_repo

  let pp_opam = pp_sexp sexp_of_opam

  let pp = pp_sexp sexp_of_t
  let load file = Cmd.load_sexp "opam duniverse" t_of_sexp file
  let save file v = Cmd.save_sexp "opam duniverse" sexp_of_t file v

end

module Dune = struct
  type repo =
    { dir: string
    ; upstream: string
    ; ref: string [@default "master"] [@sexp_drop_default] }
  [@@deriving sexp]

  type t = {repos: repo list} [@@deriving sexp]

  let pp_repo = pp_sexp sexp_of_repo

  let pp = pp_sexp sexp_of_t
  let load file = Cmd.load_sexp "git duniverse" t_of_sexp file 
  let save file v = Cmd.save_sexp "git duniverse" sexp_of_t file v
end
