open Import

module Switch_and_local_packages_context : sig
  type r =
    | Non_dune
    | Switch_rejection of Opam_0install.Switch_context.rejection

  include Opam_0install.S.CONTEXT with type rejection = r

  val create :
    ?install_test_deps_for:OpamPackage.Name.Set.t ->
    allow_jbuilder:bool ->
    fixed_packages:
      (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    OpamStateTypes.unlocked OpamStateTypes.switch_state ->
    t
end = struct
  type t = {
    switch_context : Opam_0install.Switch_context.t;
    fixed_packages :
      (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
    allow_jbuilder : bool;
  }

  type r =
    | Non_dune
    | Switch_rejection of Opam_0install.Switch_context.rejection

  type rejection = r

  let pp_rejection fmt = function
    | Non_dune -> Fmt.pf fmt "Doesn't build with dune"
    | Switch_rejection r -> Opam_0install.Switch_context.pp_rejection fmt r

  let create ?install_test_deps_for ~allow_jbuilder ~fixed_packages ~constraints
      switch_state =
    let switch_context =
      Opam_0install.Switch_context.create ?test:install_test_deps_for
        ~constraints switch_state
    in
    { switch_context; fixed_packages; allow_jbuilder }

  let is_valid_candidate ~allow_jbuilder ~name ~version opam_file =
    let pkg = OpamPackage.create name version in
    let depends = OpamFile.OPAM.depends opam_file in
    let depopts = OpamFile.OPAM.depopts opam_file in
    let uses_dune =
      Opam.depends_on_dune ~allow_jbuilder depends
      || Opam.depends_on_dune ~allow_jbuilder depopts
    in
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

  let candidates { switch_context; fixed_packages; allow_jbuilder } name =
    match OpamPackage.Name.Map.find_opt name fixed_packages with
    | Some (version, opam_file) -> [ (version, Ok opam_file) ]
    | None ->
        Opam_0install.Switch_context.candidates switch_context name
        |> filter_candidates ~allow_jbuilder ~name

  let user_restrictions { switch_context; _ } name =
    Opam_0install.Switch_context.user_restrictions switch_context name

  let filter_deps { switch_context; _ } pkg formula =
    Opam_0install.Switch_context.filter_deps switch_context pkg formula
end

module Local_solver =
  Opam_0install.Solver.Make (Switch_and_local_packages_context)

let constraints ~ocaml_version =
  let no_constraints = OpamPackage.Name.Map.empty in
  match ocaml_version with
  | Some version ->
      let key = OpamPackage.Name.of_string "ocaml" in
      let value = (`Eq, OpamPackage.Version.of_string version) in
      OpamPackage.Name.Map.safe_add key value no_constraints
  | None -> no_constraints

let request ~allow_compiler_variants local_packages_names =
  if allow_compiler_variants then local_packages_names
  else
    (* We add ocaml-base-compiler to the solver request to prevent it
       from selecting a version of OCaml that hasn't been officially
       released yet but that exists in opam with variants such as
       ocaml-variants.x+trunk *)
    let base_compiler = OpamPackage.Name.of_string "ocaml-base-compiler" in
    base_compiler :: local_packages_names

let depend_on_compiler_variants local_packages =
  OpamPackage.Name.Map.exists
    (fun _name (_version, opam_file) ->
      let depends = OpamFile.OPAM.depends opam_file in
      Opam.depends_on_compiler_variants depends)
    local_packages

exception Pinned_local_package

let fixed_packages ~local_packages ~pin_depends =
  try
    Ok
      (OpamPackage.Name.Map.union
         (fun _local _pin -> raise Pinned_local_package)
         local_packages pin_depends)
  with Pinned_local_package ->
    Rresult.R.error_msg
      "You have a locally defined package in a pin-depends field of another \
       locally defined package"

let calculate_raw ~build_only ~allow_jbuilder ~ocaml_version ~local_packages
    ~target_packages ~pin_depends switch_state =
  let open Rresult.R.Infix in
  let target_packages_names = OpamPackage.Name.Set.elements target_packages in
  let install_test_deps_for =
    if build_only then OpamPackage.Name.Set.empty else target_packages
  in
  let constraints = constraints ~ocaml_version in
  fixed_packages ~local_packages ~pin_depends >>= fun fixed_packages ->
  let context =
    Switch_and_local_packages_context.create ~install_test_deps_for
      ~allow_jbuilder ~constraints ~fixed_packages switch_state
  in
  let allow_compiler_variants = depend_on_compiler_variants local_packages in
  let request = request ~allow_compiler_variants target_packages_names in
  let result = Local_solver.solve context request in
  match result with
  | Error e -> Error (`Diagnostics e)
  | Ok selections ->
      let packages = Local_solver.packages_of_result selections in
      let deps =
        List.filter
          ~f:(fun pkg ->
            let name = OpamPackage.name pkg in
            let in_local_packages =
              OpamPackage.Name.Map.mem name local_packages
            in
            not in_local_packages)
          packages
      in
      Ok deps

type diagnostics = Local_solver.diagnostics

let diagnostics_message ~verbose diagnostics =
  `Msg (Local_solver.diagnostics ~verbose diagnostics)

module Pkg_map = Local_solver.Solver.Output.RoleMap

let no_version_builds_with_dune component =
  let rejects, _reason = Local_solver.Diagnostics.Component.rejects component in
  match rejects with
  | [] -> false
  | _ ->
      List.for_all
        ~f:(fun (_, reason) ->
          match reason with
          | `Model_rejection Switch_and_local_packages_context.Non_dune -> true
          | _ -> false)
        rejects

let not_buildable_with_dune diagnostics =
  let rolemap = Local_solver.diagnostics_rolemap diagnostics in
  Pkg_map.fold
    (fun pkg component acc ->
      match no_version_builds_with_dune component with
      | false -> acc
      | true -> pkg :: acc)
    rolemap []
  |> List.filter_map ~f:Local_solver.package_name

let get_opam_info ~pin_depends ~switch_state pkg =
  let opam_file =
    match OpamPackage.Name.Map.find_opt pkg.OpamPackage.name pin_depends with
    | Some (_version, opam_file) -> opam_file
    | None -> OpamSwitchState.opam switch_state pkg
  in
  Opam.Package_summary.from_opam ~pkg opam_file

(* TODO catch exceptions and turn to error *)

let calculate ~build_only ~allow_jbuilder ~local_opam_files ~target_packages
    ~pin_depends ?ocaml_version switch_state =
  let open Rresult.R.Infix in
  calculate_raw ~build_only ~allow_jbuilder ~ocaml_version
    ~local_packages:local_opam_files ~target_packages ~pin_depends switch_state
  >>= fun deps ->
  Logs.app (fun l ->
      l "%aFound %a opam dependencies for the target package%a."
        Pp.Styled.header ()
        Fmt.(styled `Green int)
        (List.length deps) Pp.plural_int
        (OpamPackage.Name.Set.cardinal target_packages));
  Logs.info (fun l ->
      l "The dependencies are: %a"
        Fmt.(list ~sep:(any ",@ ") Opam.Pp.package)
        deps);
  Logs.app (fun l ->
      l "%aQuerying opam database for their metadata and Dune compatibility."
        Pp.Styled.header ());
  Ok (List.map ~f:(get_opam_info ~pin_depends ~switch_state) deps)
