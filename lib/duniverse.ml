open Stdune
open Sexplib.Conv

module Element = struct
  type t =
    | Opam of { name : string; version : string option [@default None] [@sexp_drop_default] }
    | Repo of
        { dir : string;
          upstream : string;
          ref : string [@default "master"] [@sexp_drop_default]
        }
  [@@deriving sexp]

  let equal t t' =
    match (t, t') with
    | Opam { name; version }, Opam { name = name'; version = version' } ->
        String.equal name name' && Option.equal String.equal version version'
    | Repo { dir; upstream; ref }, Repo { dir = dir'; upstream = upstream'; ref = ref' } ->
        String.equal dir dir' && String.equal upstream upstream' && String.equal ref ref'
    | (Opam _ | Repo _), _ -> false

  let pp fmt t =
    let pp_version fmt = function
      | None -> Format.fprintf fmt "None"
      | Some v -> Format.fprintf fmt "Some %S" v
    in
    match t with
    | Opam { name; version } ->
        Format.fprintf fmt "@[<hov 2>{ name = %S;@ version = %a }@]" name pp_version version
    | Repo { dir; upstream; ref } ->
        Format.fprintf fmt "@[<hov 2>{ dir = %S;@ upstream = %S;@ ref = %S }@]" dir upstream ref

  let from_opam_entry ~get_default_branch entry =
    let open Types.Opam in
    let open Rresult.R.Infix in
    match entry with
    | { dev_repo = `Virtual; _ } | { dev_repo = `Error _; _ } -> Ok None
    | { is_dune = false; package = { name; version }; _ } -> Ok (Some (Opam { name; version }))
    | { is_dune = true; dev_repo = `Git upstream; tag = Some ref; package = { name; _ } } ->
        Ok (Some (Repo { dir = name; upstream; ref }))
    | { is_dune = true; dev_repo = `Git upstream; tag = None; package = { name; _ } } ->
        get_default_branch upstream >>= fun ref -> Ok (Some (Repo { dir = name; upstream; ref }))

  let dedup_upstream l =
    let merge_repo t t' =
      match[@warning "-4"] (t, t') with
      | Repo { dir; ref; upstream }, Repo { dir = dir'; ref = ref'; _ } ->
          let min_dir = match String.compare dir dir' with Lt | Eq -> dir | Gt -> dir' in
          let max_ref =
            match Ordering.of_int (OpamVersionCompare.compare ref ref') with
            | Gt | Eq -> ref
            | Lt -> ref'
          in
          Repo { dir = min_dir; ref = max_ref; upstream }
      | _ -> assert false
    in
    let update map upstream t =
      String.Map.update map upstream ~f:(function
        | None -> Some t
        | Some t' -> Some (merge_repo t t') )
    in
    let go (opams, upstream_repo_map) t =
      match t with
      | Opam _ -> (t :: opams, upstream_repo_map)
      | Repo { upstream; _ } -> (opams, update upstream_repo_map upstream t)
    in
    let opams, upstream_repo_map = List.fold_left l ~init:([], String.Map.empty) ~f:go in
    let repos = String.Map.values upstream_repo_map in
    List.rev_append opams repos
end

type t = {
  root_packages : Types.Opam.package list;
  excludes : Types.Opam.package list;
  pins : Types.Opam.pin list;
  remotes : string list; [@default []]
  branch : string; [@default "master"]
  content : Element.t list
}
[@@deriving sexp]

let load ~file = Persist.load_sexp "duniverse" t_of_sexp file

let save ~file t = Persist.save_sexp "duniverse" sexp_of_t file t
