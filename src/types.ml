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

  type package =
    {name: string; version: string option [@default None] [@sexp_drop_default]}
  [@@deriving sexp]

  type entry =
    { package: package
    ; dev_repo: repo
    ; tag: string option [@default None] [@sexp_drop_default]
    ; is_dune: bool [@default true] [@sexp_drop_default] }
  [@@deriving sexp]

  type t =
    { roots: package list
    ; excludes: package list
    ; pins: string list
    ; pkgs: entry list
    ; remotes: string list [@default []]
    ; branch: string [@default "master"]
    ; opam_switch: string }
  [@@deriving sexp]

  let pp_repo = pp_sexp sexp_of_repo

  let pp_package ppf {name; version} =
    match version with
    | None -> Fmt.pf ppf "%s" name
    | Some v -> Fmt.pf ppf "%s.%s" name v

  let string_of_package pkg = Fmt.strf "%a" pp_package pkg

  let pp_entry = pp_sexp sexp_of_entry

  let pp = pp_sexp sexp_of_t

  let load file = Cmd.load_sexp "opam duniverse" t_of_sexp file

  let save file v = Cmd.save_sexp "opam duniverse" sexp_of_t file v

  let sort_uniq l = List.sort_uniq (fun a b -> String.compare a.name b.name) l
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
