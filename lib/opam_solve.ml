open Import

module type BASE_CONTEXT = sig
  include Opam_0install.S.CONTEXT

  type input

  val create :
    ?test:OpamPackage.Name.Set.t ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    input ->
    t
end

module type OPAM_MONOREPO_CONTEXT = sig
  type input

  type base_rejection

  type r = Non_dune | Base_rejection of base_rejection

  include Opam_0install.S.CONTEXT with type rejection = r

  val create :
    ?install_test_deps_for:OpamPackage.Name.Set.t ->
    allow_jbuilder:bool ->
    fixed_packages:
      (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    constraints:OpamFormula.version_constraint OpamTypes.name_map ->
    input ->
    t

  val opam_file : t -> OpamPackage.t -> (OpamFile.OPAM.t, Rresult.R.msg) result
  (** Convenience function to return the opam file associated to a pkg
    in the given context.
    Takes into account local packages an pin-depends. *)
end

module Opam_monorepo_context (Base_context : BASE_CONTEXT) :
  OPAM_MONOREPO_CONTEXT
    with type base_rejection = Base_context.rejection
     and type input = Base_context.input = struct
  type base_rejection = Base_context.rejection

  type input = Base_context.input

  type t = {
    base_context : Base_context.t;
    fixed_packages :
      (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
    allow_jbuilder : bool;
  }

  type r = Non_dune | Base_rejection of Base_context.rejection

  type rejection = r

  let pp_rejection fmt = function
    | Non_dune -> Fmt.pf fmt "Doesn't build with dune"
    | Base_rejection r -> Base_context.pp_rejection fmt r

  let create ?install_test_deps_for ~allow_jbuilder ~fixed_packages ~constraints
      input =
    let base_context =
      Base_context.create ?test:install_test_deps_for ~constraints input
    in
    { base_context; fixed_packages; allow_jbuilder }

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
        | Error r -> (version, Error (Base_rejection r))
        | Ok opam_file ->
            if is_valid_candidate ~allow_jbuilder ~name ~version opam_file then
              (version, Ok opam_file)
            else (version, Error Non_dune))
      versions

  let candidates { base_context; fixed_packages; allow_jbuilder } name =
    match OpamPackage.Name.Map.find_opt name fixed_packages with
    | Some (version, opam_file) -> [ (version, Ok opam_file) ]
    | None ->
        Base_context.candidates base_context name
        |> filter_candidates ~allow_jbuilder ~name

  let user_restrictions { base_context; _ } name =
    Base_context.user_restrictions base_context name

  let filter_deps { base_context; _ } pkg formula =
    Base_context.filter_deps base_context pkg formula

  let opam_file t pkg =
    let name = OpamPackage.name pkg in
    let candidates = candidates t name in
    let version = OpamPackage.version pkg in
    let res =
      List.find_map candidates ~f:(fun (v, opam_file) ->
          if OpamPackage.Version.equal v version then Some opam_file else None)
    in
    match res with
    | None -> Rresult.R.error_msgf "No such package %a" Opam.Pp.package pkg
    | Some (Ok opam_file) -> Ok opam_file
    | Some (Error rejection) ->
        Rresult.R.error_msgf "Package %a rejected: %a" Opam.Pp.package pkg
          pp_rejection rejection
end

exception Pinned_local_package

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

module type OPAM_MONOREPO_SOLVER = sig
  type input

  type diagnostics

  val calculate :
    build_only:bool ->
    allow_jbuilder:bool ->
    local_opam_files:
      (OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    target_packages:OpamPackage.Name.Set.t ->
    pin_depends:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    ?ocaml_version:string ->
    input ->
    ( Opam.Package_summary.t list,
      [> `Diagnostics of diagnostics | `Msg of string ] )
    result

  val diagnostics_message : verbose:bool -> diagnostics -> [> `Msg of string ]

  val not_buildable_with_dune : diagnostics -> OpamPackage.Name.t list
end

module Make_solver (Context : OPAM_MONOREPO_CONTEXT) :
  OPAM_MONOREPO_SOLVER with type input = Context.input = struct
  type input = Context.input

  module Solver = Opam_0install.Solver.Make (Context)

  let build_context ~build_only ~allow_jbuilder ~ocaml_version ~local_packages
      ~pin_depends ~target_packages input =
    let open Result.O in
    let install_test_deps_for =
      if build_only then OpamPackage.Name.Set.empty else target_packages
    in
    let constraints = constraints ~ocaml_version in
    let+ fixed_packages = fixed_packages ~local_packages ~pin_depends in
    Context.create ~install_test_deps_for ~allow_jbuilder ~constraints
      ~fixed_packages input

  let calculate_raw ~local_packages ~target_packages context =
    let target_packages_names = OpamPackage.Name.Set.elements target_packages in
    let allow_compiler_variants = depend_on_compiler_variants local_packages in
    let request = request ~allow_compiler_variants target_packages_names in
    let result = Solver.solve context request in
    match result with
    | Error e -> Error (`Diagnostics e)
    | Ok selections ->
        let packages = Solver.packages_of_result selections in
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

  type diagnostics = Solver.diagnostics

  let diagnostics_message ~verbose diagnostics =
    `Msg (Solver.diagnostics ~verbose diagnostics)

  module Pkg_map = Solver.Solver.Output.RoleMap

  let no_version_builds_with_dune component =
    match Solver.Diagnostics.Component.selected_impl component with
    | Some _ -> false
    | None -> (
        let rejects, _reason = Solver.Diagnostics.Component.rejects component in
        match rejects with
        | [] -> false
        | _ ->
            List.for_all
              ~f:(fun (_, reason) ->
                match reason with
                | `Model_rejection Context.Non_dune -> true
                | _ -> false)
              rejects)

  let not_buildable_with_dune diagnostics =
    let rolemap = Solver.diagnostics_rolemap diagnostics in
    Pkg_map.fold
      (fun pkg component acc ->
        match no_version_builds_with_dune component with
        | false -> acc
        | true -> pkg :: acc)
      rolemap []
    |> List.filter_map ~f:Solver.package_name

  let get_opam_info ~context pkg =
    match Context.opam_file context pkg with
    | Ok opam_file -> Opam.Package_summary.from_opam ~pkg opam_file
    | Error (`Msg msg) ->
        (* If we're calling this function on a package, it means it has been
           returned as part of the solver solution and therefore should correspond
           to a valid candidate. *)
        Logs.debug (fun l ->
            l "Could not retrieve opam file for %a: %s" Opam.Pp.package pkg msg);
        assert false

  (* TODO catch exceptions and turn to error *)

  let calculate ~build_only ~allow_jbuilder ~local_opam_files:local_packages
      ~target_packages ~pin_depends ?ocaml_version input =
    let open Result.O in
    let* context =
      build_context ~build_only ~allow_jbuilder ~ocaml_version ~pin_depends
        ~local_packages ~target_packages input
    in
    let* deps = calculate_raw ~local_packages ~target_packages context in
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
    Ok (List.map ~f:(get_opam_info ~context) deps)
end

type explicit_repos = string list

type opam_env = OpamVariable.variable_contents String.Map.t

type switch = OpamStateTypes.unlocked OpamStateTypes.switch_state

module Multi_dir_context :
  BASE_CONTEXT with type input = opam_env * explicit_repos = struct
  include Opam_0install.Dir_context

  type input = opam_env * explicit_repos
  (** A list of repo URLs *)

  type nonrec t = t list

  (** Create a Dir_context with multiple repos. The list is ordered by priority.
      First repo in the list as higher priority. If two repos provide the same version
      of a package, the one from the highest priority repo will be used, the other
      discared by [candidates]. *)
  let create ?test ~constraints (env, paths) =
    let env varname = String.Map.find_opt varname env in
    match paths with
    | [] ->
        invalid_arg
          "Multi_dir_context should be instanciated with at least one repo"
    | paths -> List.map ~f:(create ?test ~constraints ~env) paths

  let merge_candidates ~name acc candidates =
    List.fold_left ~init:acc candidates ~f:(fun acc (version, opam_file_res) ->
        try OpamPackage.Version.Map.safe_add version opam_file_res acc
        with Failure _ ->
          Logs.info (fun l ->
              l
                "Several of the configured repos define %s.%s. Note that for \
                 now opam-monorepo does not support defining priorities \
                 between repos and picked one arbitrarily."
                (OpamPackage.Name.to_string name)
                (OpamPackage.Version.to_string version));
          acc)

  let candidates t name =
    let map =
      List.fold_left t ~init:OpamPackage.Version.Map.empty
        ~f:(fun acc dir_context ->
          let candidates = candidates dir_context name in
          merge_candidates ~name acc candidates)
    in
    OpamPackage.Version.Map.bindings map

  let user_restrictions t pkg = user_restrictions (List.hd t) pkg

  let filter_deps t pkg f = filter_deps (List.hd t) pkg f
end

module Local_opam_context : BASE_CONTEXT with type input = switch = struct
  include Opam_0install.Switch_context

  type input = OpamStateTypes.unlocked OpamStateTypes.switch_state

  let create ?test ~constraints switch_state =
    create ?test ~constraints switch_state
end

(* The code below aims to provide a unified interface over the two solver
   modules *)

type ('a, 'b) t =
  (module OPAM_MONOREPO_SOLVER with type input = 'a and type diagnostics = 'b)

module Local_opam_config_context = Opam_monorepo_context (Local_opam_context)
module Explicit_repos_context = Opam_monorepo_context (Multi_dir_context)
module Local_opam_config_solver = Make_solver (Local_opam_config_context)
module Explicit_repos_solver = Make_solver (Explicit_repos_context)

type switch_diagnostics = Local_opam_config_solver.diagnostics

type explicit_repos_diagnostics = Explicit_repos_solver.diagnostics

let local_opam_config_solver : (switch, switch_diagnostics) t =
  (module Local_opam_config_solver)

let explicit_repos_solver :
    (opam_env * explicit_repos, explicit_repos_diagnostics) t =
  (module Explicit_repos_solver)

let calculate :
    type context diagnostics.
    build_only:bool ->
    allow_jbuilder:bool ->
    local_opam_files:
      (OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    target_packages:OpamPackage.Name.Set.t ->
    pin_depends:(OpamTypes.version * OpamFile.OPAM.t) OpamPackage.Name.Map.t ->
    ?ocaml_version:string ->
    (context, diagnostics) t ->
    context ->
    ( Opam.Package_summary.t list,
      [> `Diagnostics of diagnostics | `Msg of string ] )
    result =
 fun ~build_only ~allow_jbuilder ~local_opam_files ~target_packages ~pin_depends
     ?ocaml_version t input ->
  let (module Solver : OPAM_MONOREPO_SOLVER
        with type diagnostics = diagnostics
         and type input = context) =
    t
  in
  Solver.calculate ~build_only ~allow_jbuilder ~local_opam_files
    ~target_packages ~pin_depends ?ocaml_version input

let diagnostics_message :
    type context diagnostics.
    verbose:bool ->
    (context, diagnostics) t ->
    diagnostics ->
    [> `Msg of string ] =
 fun ~verbose t diagnostics ->
  let (module Solver : OPAM_MONOREPO_SOLVER
        with type diagnostics = diagnostics
         and type input = context) =
    t
  in
  Solver.diagnostics_message ~verbose diagnostics

let not_buildable_with_dune :
    type context diagnostics.
    (context, diagnostics) t -> diagnostics -> OpamPackage.Name.t list =
 fun t diagnostics ->
  let (module Solver : OPAM_MONOREPO_SOLVER
        with type diagnostics = diagnostics
         and type input = context) =
    t
  in
  Solver.not_buildable_with_dune diagnostics
