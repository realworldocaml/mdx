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

  type rejection = Opam_0install.Switch_context.rejection

  let pp_rejection = Opam_0install.Switch_context.pp_rejection

  let create ?test ~local_packages ~constraints switch_state =
    let switch_context = Opam_0install.Switch_context.create ?test ~constraints switch_state in
    { switch_context; local_packages }

  let candidates {switch_context; local_packages} name =
    match OpamPackage.Name.Map.find_opt name local_packages with
    | Some opam_file ->
      [ OpamPackage.Version.of_string "zdev", Ok opam_file ]
    | None ->
      Opam_0install.Switch_context.candidates switch_context name

  let user_restrictions {switch_context; _} name =
    Opam_0install.Switch_context.user_restrictions switch_context name

  let filter_deps {switch_context; _} pkg formula =
    Opam_0install.Switch_context.filter_deps switch_context pkg formula
end

module Local_solver = Opam_0install.Solver.Make(Switch_and_local_packages_context)

let calculate ~local_packages switch_state =
  let local_packages_names = OpamPackage.Name.Map.keys local_packages in
  let names_set = OpamPackage.Name.Set.of_list local_packages_names in
  let context =
    Switch_and_local_packages_context.create
      ~test:names_set
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
