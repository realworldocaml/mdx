open! Lwt.Infix

let ( / ) = Filename.concat

type package = OpamPackage.t

type selection = { variant : string; packages : package list }

let with_dir path fn =
  let ch = Unix.opendir path in
  match fn ch with
  | x ->
      Unix.closedir ch;
      x
  | exception ex ->
      Unix.closedir ch;
      raise ex

let list_dir path =
  let rec aux ch =
    match Unix.readdir ch with name -> name :: aux ch | exception End_of_file -> []
  in
  with_dir path aux

type variant = {
  id : string;
  arch : string;
  os : string;
  os_distribution : string;
  os_version : string;
  os_family : string;
  ocaml : OpamPackage.Version.t;
}

let pp_variant f x = Fmt.string f x.id

let variant ~arch ~os ~os_distribution ~os_version ~os_family ~ocaml =
  let id = Printf.sprintf "%s/%s/%s/%s/%s/%s" arch os os_family os_distribution os_version ocaml in
  let ocaml = OpamPackage.Version.of_string ocaml in
  { id; arch; os; os_distribution; os_version; os_family; ocaml }

module Context = struct
  type rejection = UserConstraint of OpamFormula.atom

  type t = {
    variant : variant;
    packages_dir : string;
    pins : (OpamPackage.Version.t * string) OpamPackage.Name.Map.t;
    (* name -> version, opam path *)
    constraints : OpamFormula.version_constraint OpamTypes.name_map;
    (* User-provided constraints *)
    test : OpamPackage.Name.Set.t;
  }

  let load t pkg =
    let { OpamPackage.name; version = _ } = pkg in
    let opam_path =
      match OpamPackage.Name.Map.find_opt name t.pins with
      | Some (_, path) -> path
      | None ->
          t.packages_dir / OpamPackage.Name.to_string name / OpamPackage.to_string pkg / "opam"
    in
    OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))

  let user_restrictions t name = OpamPackage.Name.Map.find_opt name t.constraints

  let env t pkg v =
    if List.mem v OpamPackageVar.predefined_depends_variables then None
    else
      match OpamVariable.Full.to_string v with
      | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
      | "arch" -> Some (OpamTypes.S t.variant.arch)
      | "os" -> Some (OpamTypes.S t.variant.os)
      | "os-distribution" -> Some (OpamTypes.S t.variant.os_distribution)
      | "os-version" -> Some (OpamTypes.S t.variant.os_version)
      | "os-family" -> Some (OpamTypes.S t.variant.os_family)
      | _ ->
          OpamConsole.warning "Unknown variable %S" (OpamVariable.Full.to_string v);
          None

  let filter_deps t pkg f =
    let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
    f
    |> OpamFilter.partial_filter_formula (env t pkg)
    |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev:false ~default:false

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.pins with
    | Some (version, _) -> [ (version, None) ]
    | None -> (
        match list_dir (t.packages_dir / OpamPackage.Name.to_string name) with
        | versions ->
            let user_constraints = user_restrictions t name in
            versions
            |> List.filter_map (fun dir ->
                   match OpamPackage.of_string_opt dir with
                   | Some pkg -> Some (OpamPackage.version pkg)
                   | None -> None)
            |> List.sort (fun a b -> OpamPackage.Version.compare b a)
            |> List.map (fun v ->
                   match user_constraints with
                   | Some test
                     when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v) ->
                       (v, Some (UserConstraint (name, Some test))) (* Reject *)
                   | _ -> (v, None))
        | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
            OpamConsole.log "opam-zi" "Package %S not found!" (OpamPackage.Name.to_string name);
            [] )

  let pp_rejection f = function
    | UserConstraint x ->
        Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)
end

module Input = Opam_zi.Model (Context)
module Solver = Zeroinstall_solver.Make (Input)

let requirements ~context pkgs =
  let role =
    match pkgs with
    | [ pkg ] -> Input.role context pkg
    | pkgs ->
        let impl = Input.virtual_impl ~context ~depends:pkgs () in
        Input.virtual_role [ impl ]
  in
  { Input.role; command = None }

let dev = OpamPackage.Version.of_string "dev"

let pp_sel f pkg = Fmt.string f (OpamPackage.to_string pkg)

let ocaml_name = OpamPackage.Name.of_string "ocaml"

let solve ~opam_repo ~opam_files src ~variants =
  let src = Fpath.to_string src in
  let opam_repository = Fpath.to_string opam_repo in
  let pkgs =
    opam_files
    |> List.map (fun path ->
           let name =
             Filename.basename path |> Filename.chop_extension |> OpamPackage.Name.of_string
           in
           let version =
             let dir = Filename.dirname path in
             if dir = "." then dev
             else
               match OpamPackage.of_string_opt dir with
               | Some { OpamPackage.version; _ } -> version
               | None -> dev
           in
           (OpamPackage.create name version, path))
  in
  let root_pkgs =
    pkgs
    |> List.filter_map (fun (pkg, path) ->
           if Filename.dirname path = "." then Some (OpamPackage.name pkg) else None)
  in
  let pins =
    pkgs
    |> List.map (fun (pkg, path) -> (OpamPackage.name pkg, (OpamPackage.version pkg, src / path)))
    |> OpamPackage.Name.Map.of_list
  in
  (* let pp_name f name = Fmt.string f (OpamPackage.Name.to_string name) in *)
  (* Fmt.pr "Solving for %a\n%!" (Fmt.(list ~sep:comma) pp_name) root_pkgs; *)
  variants
  |> Lwt_list.filter_map_s (fun variant ->
         (* Fmt.pr "= %a =\n%!" pp_variant variant; *)
         let context =
           {
             Context.variant;
             packages_dir = opam_repository / "packages";
             pins;
             constraints = OpamPackage.Name.Map.singleton ocaml_name (`Eq, variant.ocaml);
             test = OpamPackage.Name.Set.of_list root_pkgs;
           }
         in
         let req = requirements ~context root_pkgs in
         let _t0 = Unix.gettimeofday () in
         let r = Solver.do_solve ~closest_match:false req in
         let _t1 = Unix.gettimeofday () in
         match r with
         | Some sels ->
             (* Fmt.pr "Solve succeeded in %.2f s\n%!" (t1 -. t0); *)
             let pkgs =
               sels |> Solver.Output.to_map |> Solver.Output.RoleMap.to_seq |> List.of_seq
               |> List.filter_map (fun (_role, sel) -> Input.version (Solver.Output.unwrap sel))
             in
             (* Fmt.pr "-> @[<hov>%a@]" Fmt.(list ~sep:sp pp_sel) pkgs; *)
             Lwt.return_some { variant = variant.id; packages = pkgs }
         | None ->
             (* Fmt.pr "Eliminated all possibilities in %.2f s\n%!" (t1 -. t0); *)
             Lwt.return_none)

let calculate_t ~opam_repo ~root_packages =
  let opam_files = List.map (fun {Types.Opam.name;_} -> name ^ ".opam") root_packages in
  let src = Fpath.v "." in
  let module OR = Osrelease in
  let variants =
    [
      variant
        ~arch:(OR.Arch.to_string `X86_64)
        ~os:(OR.OS.to_string `Linux)
        ~os_distribution:(OR.Distro.to_string (`Linux `Debian))
        ~os_version:"10.0"
        ~os_family:(OR.Distro.to_string (`Linux `Debian))
        ~ocaml:"4.10.0";
    ]
  in
  let open Lwt.Infix in
  solve ~opam_repo ~opam_files src ~variants >>= function
  | [ s ] -> Lwt.return s.packages
  | _ -> Lwt.fail (Failure "too many results from solver")

let calculate ~opam_repo ~root_packages =
  try Ok (Lwt_main.run (calculate_t ~opam_repo ~root_packages)) with exn -> Error (`Msg (Printexc.to_string exn))
