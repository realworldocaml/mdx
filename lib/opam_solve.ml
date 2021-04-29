open Import

module Switch_and_local_packages_context : sig
  include Opam_0install.S.CONTEXT

  val create :
    ?test:OpamPackage.Name.Set.t ->
    allow_jbuilder:bool ->
    local_packages:(OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    OpamStateTypes.unlocked OpamStateTypes.switch_state ->
    t
end = struct
  type t = {
    switch_context : Opam_0install.Switch_context.t;
    local_packages : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
    allow_jbuilder : bool;
  }

  type rejection = Non_dune | Switch_rejection of Opam_0install.Switch_context.rejection

  let pp_rejection fmt = function
    | Non_dune -> Fmt.pf fmt "Doesn't build with dune"
    | Switch_rejection r -> Opam_0install.Switch_context.pp_rejection fmt r

  let create ?test ~allow_jbuilder ~local_packages ~constraints switch_state =
    let switch_context = Opam_0install.Switch_context.create ?test ~constraints switch_state in
    { switch_context; local_packages; allow_jbuilder }

  let is_valid_candidate ~allow_jbuilder ~name ~version opam_file =
    let pkg = OpamPackage.create name version in
    let depends = OpamFile.OPAM.depends opam_file in
    let uses_dune = Opam.depends_on_dune ~allow_jbuilder depends in
    let summary = Opam.Package_summary.from_opam ~pkg opam_file in
    Opam.Package_summary.is_base_package summary
    || Opam.Package_summary.is_virtual summary
    || uses_dune

  let filter_candidates ~allow_jbuilder ~name versions =
    List.map
      ~f:(fun (version, result) ->
        match result with
        | Error r -> (version, Error (Switch_rejection r))
        | Ok opam_file ->
            if is_valid_candidate ~allow_jbuilder ~name ~version opam_file then
              (version, Ok opam_file)
            else (version, Error Non_dune))
      versions

  let candidates { switch_context; local_packages; allow_jbuilder } name =
    match OpamPackage.Name.Map.find_opt name local_packages with
    | Some (version, opam_file) -> [ (version, Ok opam_file) ]
    | None ->
        Opam_0install.Switch_context.candidates switch_context name
        |> filter_candidates ~allow_jbuilder ~name

  let user_restrictions { switch_context; _ } name =
    Opam_0install.Switch_context.user_restrictions switch_context name

  let filter_deps { switch_context; _ } pkg formula =
    Opam_0install.Switch_context.filter_deps switch_context pkg formula
end

module Local_solver = Opam_0install.Solver.Make (Switch_and_local_packages_context)

let calculate_raw ~build_only ~allow_jbuilder ~ocaml_version ~local_packages switch_state =
  let local_packages_names = OpamPackage.Name.Map.keys local_packages in
  let names_set = OpamPackage.Name.Set.of_list local_packages_names in
  let test = if build_only then OpamPackage.Name.Set.empty else names_set in
  let constraints =
    let no_constraints = OpamPackage.Name.Map.empty in
    match ocaml_version with
    | Some version ->
        let key = OpamPackage.Name.of_string "ocaml" in
        let value = (`Eq, OpamPackage.Version.of_string version) in
        OpamPackage.Name.Map.safe_add key value no_constraints
    | None -> no_constraints
  in
  let context =
    Switch_and_local_packages_context.create ~test ~allow_jbuilder ~constraints ~local_packages
      switch_state
  in
  let result = Local_solver.solve context local_packages_names in
  match result with
  | Error e -> Error (`Msg (Local_solver.diagnostics e))
  | Ok selections ->
      let packages = Local_solver.packages_of_result selections in
      let deps =
        List.filter
          ~f:(fun pkg ->
            let name = OpamPackage.name pkg in
            not (OpamPackage.Name.Set.mem name names_set))
          packages
      in
      Ok deps

let get_opam_info ~switch_state pkg =
  let opam_file = OpamSwitchState.opam switch_state pkg in
  Opam.Package_summary.from_opam ~pkg opam_file

(* TODO catch exceptions and turn to error *)

let calculate ~build_only ~allow_jbuilder ~local_opam_files ~local_packages ?ocaml_version
    switch_state =
  let open Rresult.R.Infix in
  calculate_raw ~build_only ~allow_jbuilder ~ocaml_version ~local_packages:local_opam_files
    switch_state
  >>= fun deps ->
  Logs.app (fun l ->
      l "%aFound %a opam dependencies for %a." Pp.Styled.header ()
        Fmt.(styled `Green int)
        (List.length deps)
        Fmt.(list ~sep:(unit " ") Pp.Styled.package)
        local_packages);
  Logs.info (fun l ->
      l "The dependencies for %a are: %a"
        Fmt.(list ~sep:(unit ",@ ") Types.Opam.pp_package)
        local_packages
        Fmt.(list ~sep:(unit ",@ ") Opam.Pp.package)
        deps);
  Logs.app (fun l ->
      l "%aQuerying opam database for their metadata and Dune compatibility." Pp.Styled.header ());
  Ok (List.map ~f:(get_opam_info ~switch_state) deps)
