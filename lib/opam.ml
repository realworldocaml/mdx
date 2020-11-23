open Import

let depends_on_dune (formula : OpamTypes.filtered_formula) =
  let dune = OpamPackage.Name.of_string "dune" in
  let jbuilder = OpamPackage.Name.of_string "jbuilder" in
  let is_duneish name =
    let eq n n' = OpamPackage.Name.compare n n' = 0 in
    eq dune name || eq jbuilder name
  in
  OpamFormula.fold_left (fun acc (name, _) -> acc || is_duneish name) false formula

let pull_tree ~url ~dir global_state =
  let dir_str = Fpath.to_string dir in
  let cache_dir = OpamRepositoryPath.download_cache global_state.OpamStateTypes.root in
  let label = dir_str in
  (* Opam requires a label for the pull, it's only used for logging *)
  let opam_dir = OpamFilename.Dir.of_string dir_str in
  let checksums = [] in
  let job = OpamRepository.pull_tree ~cache_dir label opam_dir checksums [ url ] in
  let result = OpamProcess.Job.run job in
  match result with
  | Result _ | Up_to_date _ -> Ok ()
  | Not_available (_, long_msg) -> Error (`Msg long_msg)

module Url = struct
  type t = Git of { repo : string; ref : string option } | Other of string

  (* This includes archives, other VCS and rsync opam src URLs *)

  let equal t t' =
    match (t, t') with
    | Git { repo; ref }, Git { repo = repo'; ref = ref' } ->
        String.equal repo repo' && Option.equal String.equal ref ref'
    | Other s, Other s' -> String.equal s s'
    | _ -> false

  let pp fmt t =
    let open Pp_combinators.Ocaml in
    match t with
    | Git { repo; ref } ->
        Format.fprintf fmt "@[<hov 2>Git@ @[<hov 2>{ repo = %a;@ ref = %a }@]@]" string repo
          (option ~brackets:true string) ref
    | Other s -> Format.fprintf fmt "@[<hov 2>Other@ %a@]" string s

  let from_opam url =
    match url.OpamUrl.backend with
    | `git -> (
        let str_url = OpamUrl.to_string url in
        match String.lsplit2 ~on:'#' str_url with
        | Some (repo, ref) -> Git { repo; ref = Some ref }
        | None -> Git { repo = str_url; ref = None } )
    | _ -> Other (OpamUrl.to_string url)

  let from_opam_field url =
    let url = OpamFile.URL.url url in
    from_opam url
end

module Package_summary = struct
  type t = { name : string; version : string; url_src : Url.t option; dev_repo : string option }

  let equal t t' =
    String.equal t.name t'.name && String.equal t.version t'.version
    && Option.equal Url.equal t.url_src t'.url_src
    && Option.equal String.equal t.dev_repo t'.dev_repo

  let pp fmt t =
    let open Pp_combinators.Ocaml in
    Format.fprintf fmt "@[<hov 2>{ name = %a;@ version = %a;@ url_src = %a;@ dev_repo = %a }@]"
      string t.name string t.version (option ~brackets:true Url.pp) t.url_src
      (option ~brackets:true string) t.dev_repo

  let from_opam ~pkg opam_file =
    let name = OpamPackage.name_to_string pkg in
    let version = OpamPackage.version_to_string pkg in
    let url_src = Option.map ~f:Url.from_opam_field (OpamFile.OPAM.url opam_file) in
    let dev_repo = Option.map ~f:OpamUrl.to_string (OpamFile.OPAM.dev_repo opam_file) in
    { name; version; url_src; dev_repo }

  let is_virtual = function
    | { url_src = None; _ } -> true
    | { dev_repo = None | Some ""; _ } -> true
    | _ -> false

  let is_base_package = function
    | { name; _ } when List.mem name ~set:Config.base_packages -> true
    | _ -> false
end

module Pp = struct
  let package fmt p = Format.fprintf fmt "%s" (OpamPackage.to_string p)
end
