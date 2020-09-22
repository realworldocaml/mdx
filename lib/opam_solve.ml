module Switch_and_local_packages_context : sig
  include Opam_0install.S.CONTEXT

  val create :
    ?test:OpamPackage.Name.Set.t ->
    local_packages:OpamFile.OPAM.t OpamPackage.Name.Map.t ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    OpamStateTypes.unlocked OpamStateTypes.switch_state ->
    t
end = struct
  type t =
    { switch_context : Opam_0install.Switch_context.t
    ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
    }

  type rejection =
    | Non_dune
    | Switch_rejection of Opam_0install.Switch_context.rejection

  let pp_rejection fmt = function
    | Non_dune -> Fmt.pf fmt "Doesn't build with dune"
    | Switch_rejection r -> Opam_0install.Switch_context.pp_rejection fmt r

  let create ?test ~local_packages ~constraints switch_state =
    let switch_context = Opam_0install.Switch_context.create ?test ~constraints switch_state in
    { switch_context; local_packages }

  let is_valid_candidate ~name ~version opam_file =
    let opackage = OpamPackage.create name version in
    let package = Types.Opam.package_from_opam opackage in
    let archive =
      Stdext.Option.map
        ~f:(fun x -> OpamUrl.to_string (OpamFile.URL.url x))
        (OpamFile.OPAM.url opam_file)
    in
    let dev_repo = Stdext.Option.map ~f:OpamUrl.to_string (OpamFile.OPAM.dev_repo opam_file) in
    match Opam.classify_package ~package ~archive ~dev_repo () with
    | `Virtual, _ -> true
    | _, _ ->
      Opam.depends_on_dune (OpamFile.OPAM.depends opam_file)

  let filter_candidates ~name versions =
    List.map
      (fun (version, result) ->
         match result with
         | Error r -> (version, Error (Switch_rejection r))
         | Ok opam_file ->
           if is_valid_candidate ~name ~version opam_file then
             (version, Ok opam_file)
           else
             (version, Error Non_dune))
      versions

  let candidates {switch_context; local_packages} name =
    match OpamPackage.Name.Map.find_opt name local_packages with
    | Some opam_file ->
      [ OpamPackage.Version.of_string "zdev", Ok opam_file ]
    | None ->
      Opam_0install.Switch_context.candidates switch_context name
      |> filter_candidates ~name

  let user_restrictions {switch_context; _} name =
    Opam_0install.Switch_context.user_restrictions switch_context name

  let filter_deps {switch_context; _} pkg formula =
    Opam_0install.Switch_context.filter_deps switch_context pkg formula
end

module Local_solver = Opam_0install.Solver.Make(Switch_and_local_packages_context)

let calculate ~build_only ~local_packages switch_state =
  let local_packages_names = OpamPackage.Name.Map.keys local_packages in
  let names_set = OpamPackage.Name.Set.of_list local_packages_names in
  let test = if build_only then OpamPackage.Name.Set.empty else names_set in
  let context =
    Switch_and_local_packages_context.create
      ~test
      ~constraints:OpamPackage.Name.Map.empty
      ~local_packages
      switch_state
  in
  let result = Local_solver.solve context local_packages_names in
  match result with
  | Error e -> Error (`Msg (Local_solver.diagnostics e))
  | Ok selections ->
    let packages = Local_solver.packages_of_result selections in
    let deps =
      List.filter
        (fun pkg ->
           let name = OpamPackage.name pkg in
           not (OpamPackage.Name.Set.mem name names_set))
        packages
    in
    Ok deps
