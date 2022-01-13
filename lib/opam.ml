open Import

let depends_on_any packages (formula : OpamTypes.filtered_formula) =
  let packages = List.map ~f:OpamPackage.Name.of_string packages in
  let is_one_of_packages name =
    List.exists packages ~f:(fun p -> OpamPackage.Name.compare p name = 0)
  in
  OpamFormula.fold_left
    (fun acc (name, _) -> acc || is_one_of_packages name)
    false formula

let ocaml_options =
  (* ocaml-options-vanilla is purposefully excluded from that list as it doesn't imply
     a dependency towards ocaml-variants on its own. *)
  [
    "ocaml-option-32bit";
    "ocaml-option-afl";
    "ocaml-option-bytecode-only";
    "ocaml-option-default-unsafe-string";
    "ocaml-option-flambda";
    "ocaml-option-fp";
    "ocaml-option-musl";
    "ocaml-option-nnp";
    "ocaml-option-nnpchecker";
    "ocaml-option-no-flat-float-array";
    "ocaml-option-spacetime";
    "ocaml-option-static";
    "ocaml-options-only-afl";
    "ocaml-options-only-flambda";
    "ocaml-options-only-flambda-fp";
    "ocaml-options-only-fp";
    "ocaml-options-only-nnp";
    "ocaml-options-only-nnpchecker";
    "ocaml-options-only-no-flat-float-array";
  ]

let depends_on_compiler_variants formula =
  depends_on_any ("ocaml-variants" :: ocaml_options) formula

let depends_on_dune ~allow_jbuilder (formula : OpamTypes.filtered_formula) =
  let packages =
    if allow_jbuilder then [ "dune"; "jbuilder" ] else [ "dune" ]
  in
  depends_on_any packages formula

let pull_tree ~url ~hashes ~dir global_state =
  let dir_str = Fpath.to_string dir in
  let cache_dir =
    OpamRepositoryPath.download_cache global_state.OpamStateTypes.root
  in
  let label = dir_str in
  (* Opam requires a label for the pull, it's only used for logging *)
  let opam_dir = OpamFilename.Dir.of_string dir_str in
  let open OpamProcess.Job.Op in
  OpamRepository.pull_tree ~cache_dir label opam_dir hashes [ url ] @@| function
  | Result _ | Up_to_date _ -> Ok ()
  | Not_available (_, long_msg) ->
      Error (`Msg (Printf.sprintf "Failed to pull %s: %s" label long_msg))

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
        Format.fprintf fmt "@[<hov 2>Git@ @[<hov 2>{ repo = %a;@ ref = %a }@]@]"
          string repo
          (option ~brackets:true string)
          ref
    | Other s -> Format.fprintf fmt "@[<hov 2>Other@ %a@]" string s

  let from_opam url =
    match url.OpamUrl.backend with
    | `git -> (
        let str_url = OpamUrl.to_string url in
        match String.lsplit2 ~on:'#' str_url with
        | Some (repo, ref) -> Git { repo; ref = Some ref }
        | None -> Git { repo = str_url; ref = None })
    | _ -> Other (OpamUrl.to_string url)

  let from_opam_field url =
    let url = OpamFile.URL.url url in
    from_opam url
end

module Hash = struct
  let equal t t' = OpamHash.Set.(equal (singleton t) (singleton t'))

  let pp fmt t =
    let contents = OpamHash.contents t in
    let kind = OpamHash.(string_of_kind (kind t)) in
    Format.fprintf fmt "%s:%s" kind contents
end

module Depexts = struct
  let equal t t' =
    List.equal
      (fun (pkg_set, filter) (pkg_set', filter') ->
        OpamSysPkg.Set.equal pkg_set pkg_set' && filter = filter')
      t t'

  let pp fmt t =
    let pp_filter fmt filter =
      Format.fprintf fmt "%s" (OpamFilter.to_string filter)
    in
    let pp_pkg_set fmt pkg_set =
      Format.fprintf fmt "%s" (OpamSysPkg.Set.to_string pkg_set)
    in
    Format.fprintf fmt "%a"
      Pp_combinators.Ocaml.(list (pair pp_pkg_set pp_filter))
      t
end

module Pp = struct
  let package = Fmt.using OpamPackage.to_string Fmt.string

  let package_name = Fmt.using OpamPackage.Name.to_string Fmt.string

  let version = Fmt.using OpamPackage.Version.to_string Fmt.string

  let raw_package fmt pkg =
    Format.fprintf fmt "@[<hov 2>{ name = %a;@ version = %a }@]" package_name
      pkg.OpamPackage.name version pkg.version

  let hash = Hash.pp

  let url = Fmt.using OpamUrl.to_string Fmt.string
end

module Package_summary = struct
  type t = {
    package : OpamPackage.t;
    url_src : Url.t option;
    hashes : OpamHash.t list;
    dev_repo : string option;
    depexts : (OpamSysPkg.Set.t * OpamTypes.filter) list;
  }

  let equal { package; url_src; hashes; dev_repo; depexts } t' =
    OpamPackage.equal package t'.package
    && Option.equal Url.equal url_src t'.url_src
    && List.equal Hash.equal hashes t'.hashes
    && Option.equal String.equal dev_repo t'.dev_repo
    && Depexts.equal depexts t'.depexts

  let pp fmt { package; url_src; hashes; dev_repo; depexts } =
    let open Pp_combinators.Ocaml in
    Format.fprintf fmt
      "@[<hov 2>{ name = %a;@ version = %a;@ url_src = %a;@ hashes = %a;@ \
       dev_repo = %a;@ depexts = %a }@]"
      Pp.package_name package.name Pp.version package.version
      (option ~brackets:true Url.pp)
      url_src (list Hash.pp) hashes
      (option ~brackets:true string)
      dev_repo Depexts.pp depexts

  let from_opam ~pkg:package opam_file =
    let url_field = OpamFile.OPAM.url opam_file in
    let url_src = Option.map ~f:Url.from_opam_field url_field in
    let hashes =
      Option.map_default ~default:[] ~f:OpamFile.URL.checksum url_field
    in
    let dev_repo =
      Option.map ~f:OpamUrl.to_string (OpamFile.OPAM.dev_repo opam_file)
    in
    let depexts = OpamFile.OPAM.depexts opam_file in
    { package; url_src; hashes; dev_repo; depexts }

  let is_virtual = function
    | { url_src = None; _ } -> true
    | { dev_repo = None | Some ""; _ } -> true
    | _ -> false

  let is_base_package = function
    | { package; _ }
      when OpamPackage.Name.Set.mem package.name Config.base_packages ->
        true
    | _ -> false
end

let local_package_version opam_file ~explicit_version =
  match explicit_version with
  | Some v -> v
  | None ->
      let default = OpamPackage.Version.of_string "zdev" in
      Option.value (OpamFile.OPAM.version_opt opam_file) ~default

module Extra_field = struct
  type 'a t = {
    name : string;
    to_opam_value : 'a -> OpamParserTypes.FullPos.value;
    from_opam_value :
      OpamParserTypes.FullPos.value -> ('a, [ `Msg of string ]) result;
  }

  let make ~name ~to_opam_value ~from_opam_value =
    {
      name = Printf.sprintf "x-opam-monorepo-%s" name;
      to_opam_value;
      from_opam_value;
    }

  let name t = t.name

  let set t a opam = OpamFile.OPAM.add_extension opam t.name (t.to_opam_value a)

  let get t opam = OpamFile.OPAM.extended opam t.name t.from_opam_value
end

module Pos = struct
  module Full_pos = OpamParserTypes.FullPos

  let default = { Full_pos.filename = "None"; start = (0, 0); stop = (0, 0) }

  let from_value { Full_pos.pos; _ } = pos

  let with_default pelem = { Full_pos.pos = default; pelem }

  let errorf ~pos fmt =
    let startl, startc = pos.Full_pos.start in
    let stopl, stopc = pos.stop in
    Format.ksprintf
      (fun msg -> Error (`Msg msg))
      ("Error in opam file %s, [%d:%d]-[%d:%d]: " ^^ fmt)
      pos.filename startl startc stopl stopc

  let value_errorf ~value fmt =
    let pos = from_value value in
    errorf ~pos fmt
end

module Value = struct
  module String = struct
    let from_value value =
      match (value : OpamParserTypes.FullPos.value) with
      | { pelem = String s; _ } -> Ok s
      | { pos; _ } -> Pos.errorf ~pos "Expected a string"

    let to_value s = Pos.with_default (OpamParserTypes.FullPos.String s)
  end

  module List = struct
    let from_value ~elm_from_value value =
      match (value : OpamParserTypes.FullPos.value) with
      | { pelem = List { pelem; _ }; _ } ->
          Result.List.map ~f:elm_from_value pelem
      | { pos; _ } -> Pos.errorf ~pos "Expected a list"

    let to_value ~elm_to_value l =
      let pelem =
        OpamParserTypes.FullPos.List
          (Pos.with_default (List.map ~f:elm_to_value l))
      in
      Pos.with_default pelem
  end
end
