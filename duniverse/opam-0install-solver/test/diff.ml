open Astring

module Name = OpamPackage.Name
module Version = OpamPackage.Version

let () =
  Fmt_tty.setup_std_outputs ()

let parse_sels ~filter str =
  String.cuts ~empty:false ~sep:" " str
  |> List.map String.trim
  |> List.map OpamPackage.of_string
  |> List.to_seq
  |> OpamPackage.Set.of_seq
  |> OpamPackage.to_map
  |> Name.Map.map (fun set ->
      match Version.Set.elements set with
      | [] -> failwith "No versions!"
      | [v] -> v
      | _ -> failwith "Multiple versions!"
    )
  |> Name.Map.filter (fun name _ -> not (Name.Set.mem name filter))

let parse ~filter path =
  let ch = open_in path in
  let rec aux acc =
    match input_line ch with
    | exception End_of_file -> acc
    | line ->
      match String.cuts ~sep:"," line with
      | [ pkg; _; sel ] ->
        let sel = String.trim sel in
        let sel = if sel = "NO-SOLUTION" then Error `Failed else Ok (parse_sels ~filter sel) in
        aux @@ Name.Map.add (Name.of_string pkg) sel acc
      | _ -> Fmt.failwith "Malformed line %S" line
  in
  let pkgs = aux Name.Map.empty in
  close_in ch;
  pkgs

let pp_name = Fmt.of_to_string Name.to_string
let pp_version = Fmt.of_to_string Version.to_string

let styled f style fmt =
  fmt |> Format.kdprintf @@ fun pp ->
  Fmt.styled style (fun f () -> pp f) f ()

let pp_version_diff f (name, (a, b)) =
  match a, b with
  | None, Some b -> Fmt.pf f "%a:+%a" pp_name name pp_version b
  | Some a, None -> Fmt.pf f "%a:-%a" pp_name name pp_version a
  | Some a, Some b ->
    let style = if OpamPackage.Version.compare a b < 0 then `Green else `Red in
    styled f style "%a:%a->%a"
      pp_name name
      pp_version a
      pp_version b
  | None, None -> assert false

let pp_diff ~hide_deps f (name, diff) =
  match diff with
  | Error `Not_present, _ -> styled f `Green "%a: package added"     pp_name name
  | _, Error `Not_present -> styled f `Red   "%a: package removed"   pp_name name
  | Error `Failed, _      -> styled f `Green "%a: previously failed" pp_name name
  | _, Error `Failed      -> styled f `Red   "%a: now failing"       pp_name name
  | Ok a, Ok b ->
    let v1 = Name.Map.find name a in
    let v2 = Name.Map.find name b in
    let d = Version.compare v1 v2 in
    if d < 0 then
      styled f `Green "%a: %a -> %a" pp_name name pp_version v1 pp_version v2
    else if d > 0 then
      styled f `Red   "%a: %a -> %a" pp_name name pp_version v1 pp_version v2
    else if hide_deps then
      Fmt.pf f "%a: (only changes in dependencies)" pp_name name
    else (
      let diff =
        Name.Map.merge (fun _ a b ->
            match a, b with
            | Some a, Some b when Version.compare a b = 0 -> None
            | None, None -> None
            | _ -> Some (a, b)
          ) a b
        |> Name.Map.to_seq |> List.of_seq
      in
      Fmt.pf f "@[<hv2>%a: @,%a@]" pp_name name (Fmt.list pp_version_diff ~sep:Fmt.sp) diff
    )

let result_equal a b =
  match a, b with
  | Ok a, Ok b -> Name.Map.equal (fun a b -> Version.compare a b = 0) a b
  | _ -> a = b

let diff filter hide_deps a b =
  let filter = Name.Set.of_list (List.map Name.of_string filter) in
  let a = parse ~filter a in
  let b = parse ~filter b in
  let get = Stdlib.Option.value ~default:(Error `Not_present) in
  let diff = Name.Map.merge (fun _ a b ->
      let a = get a in
      let b = get b in
      if result_equal a b then None else Some (a, b)
    ) a b
  in
  if Name.Map.is_empty diff then
    Fmt.pr "No differences.@."
  else (
    diff |> Name.Map.iter @@ fun name diff ->
    Fmt.pr "%a@." (pp_diff ~hide_deps) (name, diff)
  )

open Cmdliner

let old_csv = Arg.(required @@ (pos 0 (some file)) None @@ info ~docv:"OLD.csv" [])
let new_csv = Arg.(required @@ (pos 1 (some file)) None @@ info ~docv:"NEW.csv" [])
let filter = Arg.(value @@ (opt (list string)) [] @@ info ~docv:"NAME" ["ignore"])
let hide_deps = Arg.(value @@ flag @@ info ~docv:"NAME" ["hide-deps"])

let cmd : unit Cmd.t =
  let doc = "compare results from dump command" in
  let info = Cmd.info "diff" ~doc in
  let term = Term.(const diff $ filter $ hide_deps $ old_csv $ new_csv) in
  Cmd.v info term

let () = exit @@ Cmd.eval cmd
