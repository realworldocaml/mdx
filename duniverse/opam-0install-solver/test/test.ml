module Solver = Opam_0install.Solver.Make(Opam_0install.Switch_context)
module Name = OpamPackage.Name
module Version = OpamPackage.Version

let () =
  Random.self_init ();
  Fmt_tty.setup_std_outputs ()

let errors = ref 0

let select_zi st spec =
  let constraints =
    spec
    |> List.filter_map (function
        | _, None -> None
        | x, Some y -> Some (x, y)
      )
    |> Name.Map.of_list in
  let context = Opam_0install.Switch_context.create ~constraints st in
  let pkgs = List.map fst spec in
  let r = Solver.solve context pkgs in
  match r with
  | Ok sels ->
    let pkgs = Solver.packages_of_result sels in
    Ok (OpamPackage.Set.of_list pkgs)
  | Error e -> Error (lazy (Solver.diagnostics e))

let select_opam st spec =
  let request = OpamSolver.request ~install:spec () in
  let names = List.map fst spec |> Name.Set.of_list in
  match OpamSolution.resolve st Install ~orphans:OpamPackage.Set.empty ~requested:names request with
  | Success s -> Ok (OpamSolver.all_packages s)
  | Conflicts c -> Error (lazy (
      OpamCudf.string_of_conflicts st.packages (OpamSwitchState.unavailable_reason st) c
      |> String.trim
    ))

let string_of_version_opt = function
  | None -> "-"
  | Some v -> Version.to_string v

let error fmt =
  incr errors;
  Fmt.pr ("%a " ^^ fmt ^^ "@.") Fmt.(styled `Red string) "[ERROR]"

let pp_change f (name, (opam, zi)) =
  Fmt.pf f "%22s: %14s -> %s@,"
    (OpamPackage.Name.to_string name)
    (string_of_version_opt opam)
    (string_of_version_opt zi)

let pp_diff f diff =
  diff |> Name.Map.iter (fun name pair ->
      match pair with
      | Some opam, Some zi ->
        if OpamPackage.Version.compare opam zi < 0 then Fmt.(styled `Green pp_change) f (name, pair)
        else Fmt.(styled `Red pp_change) f (name, pair)
      | _ -> pp_change f (name, pair)
    )

let opam_confirms st zi_results =
  let spec = OpamPackage.Set.to_seq zi_results |> List.of_seq
             |> List.map (fun { OpamPackage.name; version } ->
                 (name, Some (`Eq, version))
               )
  in
  match select_opam st spec with
  | Ok _ -> Ok ()
  | Error (lazy e) -> Error e

let compare st ~opam ~zi =
  match opam, zi with
  | Error _, Error _ -> Fmt.pr "Opam and 0install %a.@." Fmt.(styled `Green string) "agree there is no solution";
  | Error _, Ok zi ->
    begin match opam_confirms st zi with
      | Ok () ->
      Fmt.pr "opam %a, but %a@."
        Fmt.(styled `Red string) "failed to find a solution"
        Fmt.(styled `Green string) "accepts 0install's solution as valid"
      | Error e ->
        let zi = OpamPackage.Set.to_seq zi |> List.of_seq |> List.map OpamPackage.to_string |> String.concat " " in
        error "opam failed to find a solution and rejects 0install's solution:@,%s@.@.0install found: %s" e zi
    end
  | Ok _, Error (lazy e) ->
    error "opam found a solution, but 0install didn't!@,%s" e
  | Ok opam, Ok zi ->
    let module M = Name.Map in
    let map_of set =
      let m =
        set 
        |> OpamPackage.Set.to_seq
        |> List.of_seq
        |> List.map (fun { OpamPackage.name; version } -> (name, version))
        |> M.of_list
      in
      assert (M.cardinal m = OpamPackage.Set.cardinal set);
      m
    in
    let merge _name opam zi =
      match opam, zi with
      | Some opam, Some zi when Version.compare opam zi = 0 -> None
      | _ -> Some (opam, zi)
    in
    let diff =
      let opam = map_of opam in
      let zi = map_of zi in
      M.merge merge opam zi
    in
    if M.is_empty diff then (
      Fmt.pr "Opam and 0install results are %a.@." Fmt.(styled `Green string) "identical";
    ) else (
      Fmt.pr "@[<v2>Opam and 0install %a:@,%a@]"
        Fmt.(styled `Blue string) "results differ"
        pp_diff diff;
      match opam_confirms st zi with
      | Ok () -> Fmt.pr "(opam confirms 0install's selection is also valid)@."
      | Error e ->
        error "opam rejects 0install's solution:@,%s" e
    )

let test st spec =
  Fmt.pr "@.== Solving for %a ==@." (Fmt.styled `Bold Fmt.(list ~sep:(unit " ") string)) spec;
  let spec = spec |> List.map (fun s ->
      if String.contains s '.' then (
        let { OpamPackage.name; version } = OpamPackage.of_string s in
        (name, Some (`Eq, version))
      ) else (
        (Name.of_string s, None)
      )
    )
  in
  let time fn =
    let t0 = Unix.gettimeofday () in
    let r =
      try fn st spec
      with OpamStd.Sys.Exit(60) ->  (* Timeout; already reported on console *)
        Error (lazy "(timeout)")
    in
    let t1 = Unix.gettimeofday () in
    (t1 -. t0, r)
  in
  let opam_time, opam = time select_opam in
  let zi_time, zi = time select_zi in
  Fmt.pr "Opam %.2f s -> 0install %.2f s (%.1f times faster)@." opam_time zi_time (opam_time /. zi_time);
  compare st ~opam ~zi

let () =
  let t0 = Unix.gettimeofday () in
  let root = OpamStateConfig.opamroot () in
  OpamFormatConfig.init ();
  ignore (OpamStateConfig.load_defaults root);
  OpamCoreConfig.init ();
  OpamStateConfig.init ();
  OpamClientConfig.opam_init ();
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  let rt = OpamRepositoryState.load `Lock_none gt in
  let st = OpamSwitchState.load_virtual gt rt in
  let t1 = Unix.gettimeofday () in
  Fmt.pr "Opam library initialised in %.2f s@." (t1 -. t0);
  (* Some reasonable fixed tests *)
  test st ["utop"; "ocaml.4.08.1"];
  test st ["irmin-git"; "ocaml.4.08.1"];
  test st ["irmin-mirage-git"; "ocaml.4.08.1"];
  test st ["dune"; "irmin.0.10.0"; "cohttp"; "git"; "ocaml.4.04.2"];
  test st ["datakit-ci"; "ocaml.4.08.1"];
  test st ["datakit-ci"; "ocaml.4.07.1"];
  test st ["opam-core.2.0.0~rc"; "ocaml.4.09.0"];
  let available = Lazy.force st.OpamStateTypes.available_packages
                  |> OpamPackage.Set.to_seq
                  |> Seq.map OpamPackage.name
                  |> Name.Set.of_seq |> Name.Set.to_seq
                  |> Array.of_seq in
  Fmt.pr "@.-------- Trying some packages at random --------@.";
  for _ = 0 to 10 do
    let i = Random.int (Array.length available) in
    test st ["ocaml"; OpamPackage.Name.to_string available.(i)]
  done;
  Fmt.pr "@.-------- Trying some random pairs --------@.";
  for _ = 0 to 10 do
    let i = Random.int (Array.length available) in
    let j = Random.int (Array.length available) in
    test st ["ocaml"; OpamPackage.Name.to_string available.(i); OpamPackage.Name.to_string available.(j)]
  done;
  if !errors = 0 then Fmt.pr "@.All tests %a.@." Fmt.(styled `Green string) "passed"
  else (
    Fmt.pr "@.Tests failed: %a@." Fmt.(styled `Red (fmt "%d error(s)")) !errors;
    exit 1
  )
