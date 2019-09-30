open Stdune
open Sexplib.Conv

type unresolved = Git.Ref.t

type resolved = Git.Ref.resolved [@@deriving sexp]

module Deps = struct
  module Opam = struct
    type t = { name : string; version : string option [@default None] [@sexp_drop_default] }
    [@@deriving sexp]

    let equal t t' = String.equal t.name t'.name && Option.equal String.equal t.version t'.version

    let pp fmt = function
      | { name; version = None } -> Format.fprintf fmt "%s" name
      | { name; version = Some v } -> Format.fprintf fmt "%s.%s" name v

    let raw_pp fmt { name; version } =
      let open Pp_combinators.Ocaml in
      Format.fprintf fmt "@[<hov 2>{ name = %S;@ version = %a }@]" name (option string) version
  end

  module Source = struct
    module Package = struct
      type t = { opam : Opam.t; upstream : string; ref : Git.Ref.t }

      let equal t t' =
        Opam.equal t.opam t'.opam
        && String.equal t.upstream t'.upstream
        && Git.Ref.equal t.ref t'.ref

      let raw_pp fmt { opam; upstream; ref } =
        Format.fprintf fmt "@[<hov 2>{ opam = %a;@ upstream = %S;@ ref = %S }@]" Opam.raw_pp opam
          upstream ref
    end

    type 'ref t = {
      dir : string;
      upstream : string;
      ref : 'ref;
      provided_packages : Opam.t list [@default []] [@sexp_drop_default]
    }
    [@@deriving sexp]

    let equal equal_ref t t' =
      let { dir; upstream; ref; provided_packages } = t in
      let { dir = dir'; upstream = upstream'; ref = ref'; provided_packages = provided_packages' }
          =
        t'
      in
      String.equal dir dir' && String.equal upstream upstream' && equal_ref ref ref'
      && List.equal Opam.equal provided_packages provided_packages'

    let raw_pp pp_ref fmt { dir; upstream; ref; provided_packages } =
      let open Pp_combinators.Ocaml in
      Format.fprintf fmt
        "@[<hov 2>{ dir = %S;@ upstream = %S;@ ref = %a;@ provided_packages = %a }@]" dir upstream
        pp_ref ref (list Opam.raw_pp) provided_packages

    let from_package { Package.opam; upstream; ref } =
      { dir = opam.name; upstream; ref; provided_packages = [ opam ] }

    let aggregate t package =
      let package_name = package.Package.opam.name in
      let new_dir =
        match String.compare t.dir package_name with Lt | Eq -> t.dir | Gt -> package_name
      in
      let new_ref =
        match Ordering.of_int (OpamVersionCompare.compare t.ref package.ref) with
        | Gt | Eq -> t.ref
        | Lt -> package.ref
      in
      { t with
        dir = new_dir;
        ref = new_ref;
        provided_packages = package.opam :: t.provided_packages
      }

    let aggregate_packages l =
      let update map ({ Package.upstream; _ } as package) =
        String.Map.update map upstream ~f:(function
          | None -> Some (from_package package)
          | Some t -> Some (aggregate t package) )
      in
      let aggregated_map = List.fold_left ~init:String.Map.empty ~f:update l in
      String.Map.values aggregated_map

    let resolve ~resolve_ref ({ upstream; ref; _ } as t) =
      let open Result.O in
      resolve_ref ~upstream ~ref >>= fun resolved_ref -> Ok { t with ref = resolved_ref }
  end

  module Classified = struct
    type t = Opam of Opam.t | Source of Source.Package.t

    let equal t t' =
      match (t, t') with
      | Opam opam, Opam opam' -> Opam.equal opam opam'
      | Source source, Source source' -> Source.Package.equal source source'
      | (Opam _ | Source _), _ -> false

    let raw_pp fmt t =
      match t with
      | Opam opam -> Format.fprintf fmt "@[<hov 2>Opam@ %a@]" Opam.raw_pp opam
      | Source source -> Format.fprintf fmt "@[<hov 2>Source@ %a@]" Source.Package.raw_pp source

    let from_opam_entry ~get_default_branch entry =
      let open Types.Opam in
      let open Result.O in
      match entry with
      | { dev_repo = `Virtual; _ } | { dev_repo = `Error _; _ } -> Ok None
      | { is_dune = false; package = { name; version }; _ } -> Ok (Some (Opam { name; version }))
      | { is_dune = true; dev_repo = `Git upstream; tag = Some ref; package = { name; version } }
        ->
          Ok (Some (Source { opam = { name; version }; upstream; ref }))
      | { is_dune = true; dev_repo = `Git upstream; tag = None; package = { name; version } } ->
          get_default_branch upstream >>= fun ref ->
          Ok (Some (Source { opam = { name; version }; upstream; ref }))
  end

  type 'ref t = { opamverse : Opam.t list; duniverse : 'ref Source.t list } [@@deriving sexp]

  let equal equal_ref t t' =
    List.equal Opam.equal t.opamverse t'.opamverse
    && List.equal (Source.equal equal_ref) t.duniverse t'.duniverse

  let raw_pp pp_ref fmt t =
    let open Pp_combinators.Ocaml in
    Format.fprintf fmt "@[<hov 2>{ opamverse = %a;@ duniverse = %a}@]" (list Opam.raw_pp)
      t.opamverse
      (list (Source.raw_pp pp_ref))
      t.duniverse

  let from_classified (l : Classified.t list) =
    let opamverse, source_deps =
      List.partition_map ~f:(function Opam o -> Left o | Source s -> Right s) l
    in
    let duniverse = Source.aggregate_packages source_deps in
    { opamverse; duniverse }

  let classify ~get_default_branch entries =
    let open Result.O in
    let results = List.map ~f:(Classified.from_opam_entry ~get_default_branch) entries in
    Result.List.all results >>= fun dep_options -> Ok (List.filter_opt dep_options)

  let from_opam_entries ~get_default_branch entries =
    let open Result.O in
    classify ~get_default_branch entries >>= fun classified -> Ok (from_classified classified)

  let count { opamverse; duniverse } = List.length opamverse + List.length duniverse

  let resolve ~resolve_ref t =
    let open Result.O in
    Parallel.map ~f:(Source.resolve ~resolve_ref) t.duniverse |> Result.List.all
    >>= fun duniverse -> Ok { t with duniverse }
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

type t = { config : Config.t; deps : resolved Deps.t } [@@deriving sexp]

let load ~file = Persist.load_sexp "duniverse" t_of_sexp file

let sort ({ deps = { opamverse; duniverse }; _ } as t) =
  let sorted_opamverse =
    let open Deps.Opam in
    let compare opam opam' = String.compare opam.name opam'.name in
    List.sort ~compare opamverse
  in
  let sorted_duniverse =
    let open Deps.Source in
    let compare source source' = String.compare source.dir source'.dir in
    List.sort ~compare duniverse
  in
  { t with deps = { opamverse = sorted_opamverse; duniverse = sorted_duniverse } }

let save ~file t = Persist.save_sexp "duniverse" sexp_of_t file (sort t)
