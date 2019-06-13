open Stdune
open Sexplib.Conv

module Deps = struct
  module Opam = struct
    type t = { name : string; version : string option [@default None] [@sexp_drop_default] }
    [@@deriving sexp]

    let equal t t' = String.equal t.name t'.name && Option.equal String.equal t.version t'.version

    let pp fmt = function
      | { name; version = None } -> Format.fprintf fmt "%s" name
      | { name; version = Some v } -> Format.fprintf fmt "%s.%s" name v

    let raw_pp fmt { name; version } =
      let pp_version fmt = function
        | None -> Format.fprintf fmt "None"
        | Some v -> Format.fprintf fmt "Some %S" v
      in
      Format.fprintf fmt "@[<hov 2>{ name = %S;@ version = %a }@]" name pp_version version
  end

  module Source = struct
    type t = {
      dir : string;
      upstream : string;
      ref : string [@default "master"] [@sexp_drop_default]
    }
    [@@deriving sexp]

    let equal t t' =
      String.equal t.dir t'.dir && String.equal t.upstream t'.upstream && String.equal t.ref t'.ref

    let raw_pp fmt { dir; upstream; ref } =
      Format.fprintf fmt "@[<hov 2>{ dir = %S;@ upstream = %S;@ ref = %S }@]" dir upstream ref

    let aggregate t t' =
      let min_dir = match String.compare t.dir t'.dir with Lt | Eq -> t.dir | Gt -> t'.dir in
      let max_ref =
        match Ordering.of_int (OpamVersionCompare.compare t.ref t'.ref) with
        | Gt | Eq -> t.ref
        | Lt -> t'.ref
      in
      { dir = min_dir; ref = max_ref; upstream = t.upstream }

    let aggregate_list l =
      let update map ({ upstream; _ } as t) =
        String.Map.update map upstream ~f:(function
          | None -> Some t
          | Some t' -> Some (aggregate t t') )
      in
      let aggregated_map = List.fold_left ~init:String.Map.empty ~f:update l in
      String.Map.values aggregated_map
  end

  module One = struct
    type t = Opam of Opam.t | Source of Source.t

    let equal t t' =
      match (t, t') with
      | Opam opam, Opam opam' -> Opam.equal opam opam'
      | Source source, Source source' -> Source.equal source source'
      | (Opam _ | Source _), _ -> false

    let raw_pp fmt t =
      match t with
      | Opam opam -> Format.fprintf fmt "@[<hov 2>Opam@ %a@]" Opam.raw_pp opam
      | Source source -> Format.fprintf fmt "@[<hov 2>Source@ %a@]" Source.raw_pp source

    let from_opam_entry ~get_default_branch entry =
      let open Types.Opam in
      let open Result.O in
      match entry with
      | { dev_repo = `Virtual; _ } | { dev_repo = `Error _; _ } -> Ok None
      | { is_dune = false; package = { name; version }; _ } -> Ok (Some (Opam { name; version }))
      | { is_dune = true; dev_repo = `Git upstream; tag = Some ref; package = { name; _ } } ->
          Ok (Some (Source { dir = name; upstream; ref }))
      | { is_dune = true; dev_repo = `Git upstream; tag = None; package = { name; _ } } ->
          get_default_branch upstream >>= fun ref ->
          Ok (Some (Source { dir = name; upstream; ref }))

    let partition_list l =
      List.partition_map ~f:(function Opam o -> Left o | Source s -> Right s) l
  end

  type t = { opamverse : Opam.t list; duniverse : Source.t list } [@@deriving sexp]

  let equal t t' =
    List.equal Opam.equal t.opamverse t'.opamverse
    && List.equal Source.equal t.duniverse t'.duniverse

  let raw_pp fmt t =
    let pp_list pp_a fmt l =
      let pp_sep fmt () = Format.fprintf fmt ";@ " in
      Format.fprintf fmt "@[<hov 2>[@ %a]@]" (Format.pp_print_list ~pp_sep pp_a) l
    in
    Format.fprintf fmt "@[<hov 2>{ opamverse = %a;@ duniverse = %a}@]" (pp_list Opam.raw_pp)
      t.opamverse (pp_list Source.raw_pp) t.duniverse

  let from_opam_entries ~get_default_branch entries =
    let open Result.O in
    let results = List.map ~f:(One.from_opam_entry ~get_default_branch) entries in
    Result.List.all results >>= fun dep_options ->
    let deps = List.filter_opt dep_options in
    let opamverse, source_deps = One.partition_list deps in
    let duniverse = Source.aggregate_list source_deps in
    Ok { opamverse; duniverse }

  let count { opamverse; duniverse } = List.length opamverse + List.length duniverse
end

module Config = struct
  type t = {
    root_packages : Types.Opam.package list;
    excludes : Types.Opam.package list;
    pins : Types.Opam.pin list;
    remotes : string list; [@default []]
    branch : string [@default "master"]
  }
  [@@deriving sexp]
end

type t = { config : Config.t; deps : Deps.t } [@@deriving sexp]

let load ~file = Persist.load_sexp "duniverse" t_of_sexp file

let save ~file t = Persist.save_sexp "duniverse" sexp_of_t file t
