open Import

module Extra_field = struct
  include Opam.Extra_field

  let get ?file t opam =
    let open Result.O in
    let* value_opt = get t opam in
    match value_opt with
    | Some result -> Ok result
    | None ->
        let file_suffix_opt = Option.map ~f:(Printf.sprintf " %s") file in
        let file_suffix = Option.value ~default:"" file_suffix_opt in
        Error
          (`Msg
            (Printf.sprintf "Missing %s field in opam-monorepo lockfile%s"
               (name t) file_suffix))
end

module Version = struct
  type t = int * int

  let current = (0, 2)

  let pp fmt (major, minor) = Format.fprintf fmt "%d.%d" major minor

  let to_string (major, minor) = Printf.sprintf "%d.%d" major minor

  let from_string s =
    let err () =
      Error (`Msg (Format.sprintf "Invalid lockfile version: %S" s))
    in
    match String.lsplit2 ~on:'.' s with
    | None -> err ()
    | Some (major, minor) -> (
        match (int_of_string_opt major, int_of_string_opt minor) with
        | Some major, Some minor -> Ok (major, minor)
        | _ -> err ())

  let backward_compatible (major, minor) (major', minor') =
    major = major' && minor >= minor'

  let compatible t =
    (* We still support 0.1 lockfiles but we'll need to update that if we stop doing so *)
    if backward_compatible current t then Ok ()
    else
      Error
        (`Msg
          (Format.asprintf
             "Incompatible opam-monorepo lockfile version %a. Please upgrade \
              your opam-monorepo plugin."
             pp t))

  let to_opam_value t = Opam.Value.String.to_value (to_string t)

  let from_opam_value value =
    let open Result.O in
    let* str = Opam.Value.String.from_value value in
    from_string str

  let field = Extra_field.make ~name:"version" ~to_opam_value ~from_opam_value
end

module Root_packages = struct
  type t = OpamPackage.Name.Set.t

  let to_opam_value t =
    let sorted =
      t |> OpamPackage.Name.Set.elements
      |> List.map ~f:OpamPackage.Name.to_string
      |> List.sort ~cmp:String.compare
    in
    Opam.Value.List.to_value Opam.Value.String.to_value sorted

  let from_opam_value value =
    let open Result.O in
    let elm_from_value value =
      let+ str = Opam.Value.String.from_value value in
      OpamPackage.Name.of_string str
    in
    Opam.Value.List.from_value elm_from_value value
    >>| OpamPackage.Name.Set.of_list

  let field =
    Extra_field.make ~name:"root-packages" ~to_opam_value ~from_opam_value
end

module Depends = struct
  type dependency = { package : OpamPackage.t; vendored : bool }

  type t = dependency list

  let from_package_summaries l =
    List.map l ~f:(fun summary ->
        let vendored =
          (not @@ Opam.Package_summary.is_base_package summary)
          && (not @@ Opam.Package_summary.is_virtual summary)
        in
        { vendored; package = summary.package })

  let variable_equal a b =
    String.equal (OpamVariable.to_string a) (OpamVariable.to_string b)

  let from_filtered_formula formula =
    let open OpamTypes in
    let atoms = OpamFormula.ands_to_list formula in
    Result.List.map atoms ~f:(function
      | Atom (name, Atom (Constraint (`Eq, FString version))) ->
          let version = OpamPackage.Version.of_string version in
          let package = OpamPackage.create name version in
          Ok { package; vendored = false }
      | Atom
          ( name,
            And
              ( Atom (Constraint (`Eq, FString version)),
                Atom (Filter (FIdent ([], var, None))) ) )
      | Atom
          ( name,
            And
              ( Atom (Filter (FIdent ([], var, None))),
                Atom (Constraint (`Eq, FString version)) ) )
        when variable_equal var Config.vendor_variable ->
          let version = OpamPackage.Version.of_string version in
          let package = OpamPackage.create name version in
          Ok { package; vendored = true }
      | _ ->
          Error
            (`Msg
              "Invalid opam-monorepo lockfile: depends should be expressed as \
               a list equality constraints optionally with a `vendor` variable"))

  let one_to_formula { package; vendored } : OpamTypes.filtered_formula =
    let name = package.name in
    let version = package.version in
    let variable =
      OpamFormula.Atom
        (OpamTypes.Filter (OpamTypes.FIdent ([], Config.vendor_variable, None)))
    in
    let version_constraint =
      OpamFormula.Atom
        (OpamTypes.Constraint
           (`Eq, OpamTypes.FString (OpamPackage.Version.to_string version)))
    in
    let formula =
      match vendored with
      | true -> OpamFormula.And (version_constraint, variable)
      | false -> version_constraint
    in
    Atom (name, formula)

  let to_filtered_formula xs =
    let sorted =
      List.sort
        ~cmp:(fun { package; _ } { package = package'; _ } ->
          OpamPackage.compare package package')
        xs
    in
    match sorted with
    | [] -> OpamFormula.Empty
    | hd :: tl ->
        List.fold_left tl
          ~f:(fun acc dep -> OpamFormula.And (acc, one_to_formula dep))
          ~init:(one_to_formula hd)
end

module Pin_depends = struct
  type t = (OpamPackage.t * OpamUrl.t) list

  let from_duniverse l =
    let open Duniverse.Repo in
    List.concat_map l ~f:(fun { provided_packages; url; _ } ->
        let url = Url.to_opam_url url in
        List.map provided_packages ~f:(fun p -> (p, url)))

  let sort t =
    List.sort ~cmp:(fun (pkg, _) (pkg', _) -> OpamPackage.compare pkg pkg') t
end

module Duniverse_dirs = struct
  type t = (string * OpamHash.t list) OpamUrl.Map.t

  let from_duniverse l =
    let open Duniverse.Repo in
    List.fold_left l ~init:OpamUrl.Map.empty
      ~f:(fun acc { dir; url; hashes; _ } ->
        OpamUrl.Map.add (Url.to_opam_url url) (dir, hashes) acc)

  let hash_to_opam_value hash =
    Opam.Value.String.to_value (OpamHash.to_string hash)

  let hash_from_opam_value value =
    let open Result.O in
    let* str = Opam.Value.String.from_value value in
    match OpamHash.of_string_opt str with
    | Some hash -> Ok hash
    | None -> Opam.Pos.value_errorf ~value "Invalid hash: %s" str

  let from_opam_value value =
    let open OpamParserTypes.FullPos in
    let open Result.O in
    let elm_from_value value =
      let* l = Opam.Value.List.from_value Result.ok value in
      match l with
      | [ { pelem = String url; _ }; { pelem = String dir; _ } ] ->
          Ok (OpamUrl.of_string url, (dir, []))
      | [ { pelem = String url; _ }; { pelem = String dir; _ }; hashes ] ->
          let* hashes =
            Opam.Value.List.from_value hash_from_opam_value hashes
          in
          Ok (OpamUrl.of_string url, (dir, hashes))
      | _ ->
          Opam.Pos.unexpected_value_error
            ~expected:"a list [ \"url\" \"repo name\" [<hashes>] ]" value
    in
    let* bindings = Opam.Value.List.from_value elm_from_value value in
    Ok (OpamUrl.Map.of_list bindings)

  let one_to_opam_value (url, (dir, hashes)) =
    let url = Opam.Value.String.to_value (OpamUrl.to_string url) in
    let dir = Opam.Value.String.to_value dir in
    let list = Opam.Value.List.to_value Fun.id in
    match hashes with
    | [] -> list [ url; dir ]
    | _ ->
        let hashes = Opam.Value.List.to_value hash_to_opam_value hashes in
        list [ url; dir; hashes ]

  let to_opam_value t =
    let l = OpamUrl.Map.bindings t in
    Opam.Value.List.to_value one_to_opam_value l

  let field =
    Extra_field.make ~name:"duniverse-dirs" ~to_opam_value ~from_opam_value
end

module Depexts = struct
  type t = (OpamSysPkg.Set.t * OpamTypes.filter) list

  let compare_elm (pkg_set, filter) (pkg_set', filter') =
    let c = OpamSysPkg.Set.compare pkg_set pkg_set' in
    if c = 0 then compare filter filter' else c

  let all ~root_depexts ~package_summaries =
    let transitive_depexts =
      List.map
        ~f:(fun { Opam.Package_summary.depexts; _ } -> depexts)
        package_summaries
    in
    let all = root_depexts @ transitive_depexts in
    List.concat all |> List.sort_uniq ~cmp:compare_elm
end

type t = {
  version : Version.t;
  root_packages : Root_packages.t;
  depends : Depends.t;
  pin_depends : Pin_depends.t;
  duniverse_dirs : Duniverse_dirs.t;
  depexts : Depexts.t;
  source_config : Source_opam_file.config;
}

let depexts t = t.depexts

let create ~source_config ~root_packages ~package_summaries ~root_depexts
    ~duniverse () =
  let version = Version.current in
  let depends = Depends.from_package_summaries package_summaries in
  let pin_depends = Pin_depends.from_duniverse duniverse in
  let duniverse_dirs = Duniverse_dirs.from_duniverse duniverse in
  let depexts = Depexts.all ~root_depexts ~package_summaries in
  {
    version;
    root_packages;
    depends;
    pin_depends;
    duniverse_dirs;
    depexts;
    source_config;
  }

let url_to_duniverse_url url =
  let url_res = Duniverse.Repo.Url.from_opam_url url in
  Result.map_error url_res ~f:(function `Msg msg ->
      let msg =
        Printf.sprintf "Invalid-monorepo lockfile pin URL %s: %s"
          (OpamUrl.to_string url) msg
      in
      `Msg msg)

let to_duniverse { duniverse_dirs; pin_depends; _ } =
  let open Result.O in
  let packages_per_url =
    List.fold_left pin_depends ~init:OpamUrl.Map.empty
      ~f:(fun acc (package, url) ->
        OpamUrl.Map.update url (fun l -> package :: l) [] acc)
    |> OpamUrl.Map.bindings
  in
  Result.List.map packages_per_url ~f:(fun (url, provided_packages) ->
      match OpamUrl.Map.find_opt url duniverse_dirs with
      | None ->
          let msg =
            Printf.sprintf
              "Invalid opam-monorepo lockfile: Missing dir for %s in %s"
              (OpamUrl.to_string url)
              (Extra_field.name Duniverse_dirs.field)
          in
          Error (`Msg msg)
      | Some (dir, hashes) ->
          let* url = url_to_duniverse_url url in
          Ok { Duniverse.Repo.dir; url; hashes; provided_packages })

let to_opam ~opam_monorepo_cwd (t : t) =
  let open OpamFile.OPAM in
  empty
  |> with_maintainer [ "opam-monorepo" ]
  |> with_synopsis "opam-monorepo generated lockfile"
  |> with_depends (Depends.to_filtered_formula t.depends)
  |> with_pin_depends (Pin_depends.sort t.pin_depends)
  |> with_depexts t.depexts
  |> Extra_field.set Version.field t.version
  |> Extra_field.set Root_packages.field t.root_packages
  |> Extra_field.set Duniverse_dirs.field t.duniverse_dirs
  |> Source_opam_file.set_config ~opam_monorepo_cwd t.source_config

let from_opam ~opam_monorepo_cwd ?file opam =
  let open Result.O in
  let* version = Extra_field.get ?file Version.field opam in
  let* () = Version.compatible version in
  let* root_packages = Extra_field.get ?file Root_packages.field opam in
  let* depends = Depends.from_filtered_formula (OpamFile.OPAM.depends opam) in
  let pin_depends = OpamFile.OPAM.pin_depends opam in
  let* duniverse_dirs = Extra_field.get ?file Duniverse_dirs.field opam in
  let depexts = OpamFile.OPAM.depexts opam in
  let* source_config =
    Source_opam_file.extract_config ~opam_monorepo_cwd opam
  in
  Ok
    {
      version;
      root_packages;
      depends;
      pin_depends;
      duniverse_dirs;
      depexts;
      source_config;
    }

let save ~opam_monorepo_cwd ~file t =
  let opam = to_opam ~opam_monorepo_cwd t in
  Bos.OS.File.with_oc file
    (fun oc () ->
      OpamFile.OPAM.write_to_channel oc opam;
      Ok ())
    ()
  |> Result.join

let load ~opam_monorepo_cwd ~file =
  let open Result.O in
  let filename = Fpath.to_string file in
  let* opam =
    Bos.OS.File.with_ic file
      (fun ic () ->
        let filename = OpamFile.make (OpamFilename.of_string filename) in
        OpamFile.OPAM.read_from_channel ~filename ic)
      ()
  in
  from_opam ~opam_monorepo_cwd ~file:filename opam
