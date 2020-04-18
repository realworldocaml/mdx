module Context = struct
  type rejection = UserConstraint of OpamFormula.atom

  type t = {
    st : OpamStateTypes.unlocked OpamStateTypes.switch_state;           (* To load the opam files *)
    pkgs : OpamTypes.version_set OpamTypes.name_map;                    (* All available versions *)
    constraints : OpamFormula.version_constraint OpamTypes.name_map;    (* User-provided constraints *)
    test : OpamPackage.Name.Set.t;
  }

  let load t pkg =
    try OpamSwitchState.opam t.st pkg
    with Not_found ->
      failwith (Format.asprintf "Package %S not found!" (OpamPackage.to_string pkg))

  let user_restrictions t name =
    OpamPackage.Name.Map.find_opt name t.constraints

  let env t pkg v =
    if List.mem v OpamPackageVar.predefined_depends_variables then None
    else (
      let r = OpamPackageVar.resolve_switch ~package:pkg t.st v in
      if r = None then OpamConsole.warning "Unknown variable %S" (OpamVariable.Full.to_string v);
      r
    )

  let filter_deps t pkg f =
    let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
    f
    |> OpamFilter.partial_filter_formula (env t pkg)
    |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev:false ~default:false

  let candidates t name =
    let user_constraints = user_restrictions t name in
    match OpamPackage.Name.Map.find_opt name t.pkgs with
    | Some versions ->
      OpamPackage.Version.Set.to_seq versions
      |> List.of_seq
      |> List.rev       (* Higher versions are preferred. *)
      |> List.map (fun v ->
          match user_constraints with
          | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) ->
            v, Some (UserConstraint (name, Some test))  (* Reject *)
          | _ -> v, None
        )
    | None ->
      OpamConsole.log "opam-zi" "Package %S not found!" (OpamPackage.Name.to_string name);
      []

  let pp_rejection f = function
    | UserConstraint x -> Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)
end

module Input = Model.Make(Context)

let requirements ~context pkgs =
  let role =
    match pkgs with
    | [pkg] -> Input.role context pkg
    | pkgs ->
      let impl = Input.virtual_impl ~context ~depends:pkgs () in
      Input.virtual_role [impl]
  in
  { Input.role; command = None }

module Solver = Zeroinstall_solver.Make(Input)
module Diagnostics = Zeroinstall_solver.Diagnostics(Solver.Output)

type t = Context.t
type selections = Solver.Output.t
type diagnostics = Input.requirements   (* So we can run another solve *)

let create ?(test=OpamPackage.Name.Set.empty) ~constraints st =
  let pkgs = Lazy.force st.OpamStateTypes.available_packages |> OpamPackage.to_map in
  { Context.st; pkgs; constraints; test }

let solve context pkgs =
  let req = requirements ~context pkgs in
  match Solver.do_solve ~closest_match:false req with
  | Some sels -> Ok sels
  | None -> Error req

let diagnostics ?verbose req =
  Solver.do_solve req ~closest_match:true
  |> Option.get
  |> Diagnostics.get_failure_reason ?verbose

let packages_of_result sels =
  sels
  |> Solver.Output.to_map |> Solver.Output.RoleMap.to_seq |> List.of_seq
  |> List.filter_map (fun (_role, sel) -> Input.version (Solver.Output.unwrap sel))

module Model = Model.Make
module S = S
