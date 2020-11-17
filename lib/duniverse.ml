open Import
open Sexplib.Conv

type unresolved = Git.Ref.t

type resolved = Git.Ref.resolved [@@deriving sexp]

module Deps = struct
  module Opam = struct
    type t = { name : string; version : string option [@default None] [@sexp_drop_default.sexp] }
    [@@deriving sexp]

    let equal t t' = String.equal t.name t'.name && Option.equal String.equal t.version t'.version

    let pp fmt = function
      | { name; version = None } -> Format.fprintf fmt "%s" name
      | { name; version = Some v } -> Format.fprintf fmt "%s.%s" name v

    let raw_pp fmt { name; version } =
      let open Pp_combinators.Ocaml in
      Format.fprintf fmt "@[<hov 2>{ name = %S;@ version = %a }@]" name (option string) version

    let explicit_version t = Option.value ~default:"zdev" t.version

    let to_opam_dep t : OpamTypes.filtered_formula =
      let version = explicit_version t in
      let name = OpamPackage.Name.of_string t.name in
      Atom (name, Atom (Constraint (`Eq, FString version)))
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
      provided_packages : Opam.t list; [@default []] [@sexp_drop_default.sexp]
    }
    [@@deriving sexp]

    let equal equal_ref t t' =
      let { dir; upstream; ref; provided_packages } = t in
      let { dir = dir'; upstream = upstream'; ref = ref'; provided_packages = provided_packages' } =
        t'
      in
      String.equal dir dir' && String.equal upstream upstream' && equal_ref ref ref'
      && List.equal Opam.equal provided_packages provided_packages'

    let raw_pp pp_ref fmt { dir; upstream; ref; provided_packages } =
      let open Pp_combinators.Ocaml in
      Format.fprintf fmt
        "@[<hov 2>{ dir = %S;@ upstream = %S;@ ref = %a;@ provided_packages = %a }@]" dir upstream
        pp_ref ref (list Opam.raw_pp) provided_packages

    let dir_name_from_package { Opam.name; version } =
      Printf.sprintf "%s.%s" name (match version with None -> "zdev" | Some x -> x)

    let from_package { Package.opam; upstream; ref } =
      let dir = dir_name_from_package opam in
      { dir; upstream; ref; provided_packages = [ opam ] }

    let aggregate t package =
      let package_name = package.Package.opam.name in
      let new_dir =
        match Ordering.of_int (String.compare t.dir package_name) with
        | Lt | Eq -> t.dir
        | Gt -> dir_name_from_package package.Package.opam
      in
      let new_ref =
        match Ordering.of_int (OpamVersionCompare.compare t.ref package.ref) with
        | Gt | Eq -> t.ref
        | Lt -> package.ref
      in
      {
        t with
        dir = new_dir;
        ref = new_ref;
        provided_packages = package.opam :: t.provided_packages;
      }

    let aggregate_packages l =
      let update map ({ Package.upstream; _ } as package) =
        String.Map.update map upstream ~f:(function
          | None -> Some (from_package package)
          | Some t -> Some (aggregate t package))
      in
      let aggregated_map = List.fold_left ~init:String.Map.empty ~f:update l in
      String.Map.values aggregated_map

    let resolve ~resolve_ref ({ upstream; ref; _ } as t) =
      let open Result.O in
      resolve_ref ~upstream ~ref >>= fun resolved_ref -> Ok { t with ref = resolved_ref }

    let to_opam_pin_deps (t : resolved t) =
      let url = OpamUrl.of_string (Printf.sprintf "git+%s#%s" t.upstream t.ref.commit) in
      List.map t.provided_packages ~f:(fun pkg ->
          let version = Opam.explicit_version pkg in
          let opam_pkg = OpamPackage.of_string (Printf.sprintf "%s.%s" pkg.name version) in
          (opam_pkg, url))
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
      | { is_dune = true; dev_repo = `Git upstream; tag = Some ref; package = { name; version } } ->
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
  type pull_mode = Submodules | Source [@@deriving sexp]

  type t = {
    version : string;
    root_packages : Types.Opam.package list;
    pull_mode : pull_mode; [@default Source]
    ocaml_compilers : string list; [@default []]
  }
  [@@deriving sexp] [@@sexp.allow_extra_fields]
end

type t = { config : Config.t; deps : resolved Deps.t } [@@deriving sexp]

let load_dune_get ~file = Persist.load_sexp "duniverse" t_of_sexp file

let sort ({ deps = { opamverse; duniverse }; _ } as t) =
  let sorted_opamverse =
    let open Deps.Opam in
    let cmp opam opam' = String.compare opam.name opam'.name in
    List.sort ~cmp opamverse
  in
  let sorted_duniverse =
    let open Deps.Source in
    let cmp source source' = String.compare source.dir source'.dir in
    List.sort ~cmp duniverse
  in
  { t with deps = { opamverse = sorted_opamverse; duniverse = sorted_duniverse } }

let sexp_of_opamverse opamverse = Sexplib0.Sexp_conv.sexp_of_list Deps.Opam.sexp_of_t opamverse

let opamverse_of_sexp sexp = Sexplib0.Sexp_conv.list_of_sexp Deps.Opam.t_of_sexp sexp

let sexp_of_duniverse duniverse =
  Sexplib0.Sexp_conv.sexp_of_list (Deps.Source.sexp_of_t Git.Ref.sexp_of_resolved) duniverse

let duniverse_of_sexp sexp =
  Sexplib0.Sexp_conv.list_of_sexp (Deps.Source.t_of_sexp Git.Ref.resolved_of_sexp) sexp

module Opam_ext : sig
  type 'a field

  val duniverse_field : Git.Ref.resolved Deps.Source.t list field

  val opamverse_field : Deps.Opam.t list field

  val config_field : Config.t field

  val add : ('a -> Sexplib0.Sexp.t) -> 'a field -> 'a -> OpamFile.OPAM.t -> OpamFile.OPAM.t

  val get :
    ?file:string ->
    ?default:'a ->
    (Sexplib0.Sexp.t -> 'a) ->
    'a field ->
    OpamFile.OPAM.t ->
    ('a, [> `Msg of string ]) result
end = struct
  type _ field = string

  let duniverse_field = "x-duniverse-duniverse"

  let opamverse_field = "x-duniverse-opamverse"

  let config_field = "x-duniverse-config"

  let add sexp_of field a opam =
    OpamFile.OPAM.add_extension opam field (Opam_value.from_sexp (sexp_of a))

  let get ?file ?default of_sexp field opam =
    let open Result.O in
    match (OpamFile.OPAM.extended opam field (fun i -> i), default) with
    | None, Some default -> Ok default
    | None, None ->
        let file_suffix_opt = Option.map ~f:(Printf.sprintf " in %s") file in
        let file_suffix = Option.value ~default:"" file_suffix_opt in
        Error (`Msg (Printf.sprintf "Missing %s field%s" field file_suffix))
    | Some ov, _ -> Opam_value.to_sexp_strict ov >>| of_sexp
end

let to_opam (t : t) =
  let deps =
    let packages =
      let opam = t.deps.opamverse in
      let open Deps.Source in
      let source = List.concat_map t.deps.duniverse ~f:(fun s -> s.provided_packages) in
      let open Deps.Opam in
      List.sort ~cmp:(fun o o' -> String.compare o.name o'.name) (opam @ source)
    in
    match packages with
    | hd :: tl ->
        List.fold_left tl
          ~f:(fun acc pkg -> OpamFormula.(And (acc, Deps.Opam.to_opam_dep pkg)))
          ~init:(Deps.Opam.to_opam_dep hd)
    | [] -> OpamFormula.Empty
  in
  let pin_deps =
    List.concat_map t.deps.duniverse ~f:Deps.Source.to_opam_pin_deps
    |> List.sort ~cmp:(fun (p, _) (p', _) -> OpamPackage.compare p p')
  in
  let t = sort t in
  let open OpamFile.OPAM in
  empty
  |> with_maintainer [ "duniverse" ]
  |> with_synopsis "duniverse generated lockfile"
  |> with_depends deps |> with_pin_depends pin_deps
  |> Opam_ext.(add Config.sexp_of_t config_field t.config)
  |> Opam_ext.(add sexp_of_opamverse opamverse_field t.deps.opamverse)
  |> Opam_ext.(add sexp_of_duniverse duniverse_field t.deps.duniverse)

let from_opam ?file opam =
  let open Result.O in
  Opam_ext.(get ?file Config.t_of_sexp config_field opam) >>= fun config ->
  Opam_ext.(get ?file ~default:[] opamverse_of_sexp opamverse_field opam) >>= fun opamverse ->
  Opam_ext.(get ?file ~default:[] duniverse_of_sexp duniverse_field opam) >>= fun duniverse ->
  let deps = { Deps.opamverse; duniverse } in
  Ok { config; deps }

let save ~file t =
  let open Result.O in
  let opam = to_opam t in
  Bos.OS.File.with_oc file
    (fun oc () ->
      OpamFile.OPAM.write_to_channel oc opam;
      Ok ())
    ()
  >>= fun res -> res

let load ~file =
  let open Result.O in
  let filename = Fpath.to_string file in
  Bos.OS.File.with_ic file
    (fun ic () ->
      let filename = OpamFile.make (OpamFilename.of_string filename) in
      OpamFile.OPAM.read_from_channel ~filename ic)
    ()
  >>= fun opam -> from_opam ~file:filename opam
