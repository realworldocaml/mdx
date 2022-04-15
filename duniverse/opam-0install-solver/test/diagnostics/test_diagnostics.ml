module Context = Opam_0install.Dir_context
module Solver = Opam_0install.Solver.Make(Context)

let main ~dir =
  let main_pkg = OpamPackage.Name.of_string "main" in
  let constraints = OpamPackage.Name.Map.empty in
  let env = Fun.const None in
  let context = Context.create ~constraints ~env dir in
  let result = Solver.solve context [main_pkg] in
  match result with
  | Ok selections ->
    List.iter
      (fun pkg -> print_endline (OpamPackage.to_string pkg))
      (Solver.packages_of_result selections)
  | Error diagnostics ->
    print_endline (Solver.diagnostics diagnostics)

let () =
  match Sys.argv with
  | [| _ ; dir |] -> main ~dir
  | _ -> assert false
