open Import
open Duniverse

type t = {
  name : OpamPackage.Name.t;
  version : OpamPackage.Version.t;
  loc : string;
  pinned : bool;
  descr : string option;
}

(* Assumes max_len was correctly computed, otherwise the input string might be
   truncated. *)
let pad s max_len = Printf.sprintf "%-*s" max_len s

let guess_pin ~version ~loc =
  let version = OpamPackage.Version.to_string version in
  (* opam-overlays *)
  String.is_suffix ~suffix:"+dune" version
  || String.is_prefix ~prefix:"https://github.com/dune-universe" loc
  (* mirage-overlays *)
  || String.is_suffix ~suffix:"+mirage" version

let pp_name = Fmt.(styled `Bold string)

let pp_version = Fmt.(styled `Magenta string)

let pp_pin_version = Fmt.(styled `Blue string)

let pp_pin_loc ppf s =
  Fmt.pf ppf "pinned at %a" Fmt.(styled `Underline string) s

let compare_pkg x y = OpamPackage.Name.compare x.name y.name

let pp ~max_name ~max_version ~short ppf t =
  if short then Duniverse_lib.Opam.Pp.package_name ppf t.name
  else
    let padded_name = pad (OpamPackage.Name.to_string t.name) max_name in
    let padded_version =
      pad (OpamPackage.Version.to_string t.version) max_version
    in
    if t.pinned then
      Fmt.pf ppf "%a %a %a" pp_name padded_name pp_pin_version padded_version
        pp_pin_loc t.loc
    else
      Fmt.pf ppf "%a %a %s" pp_name padded_name pp_version padded_version
        (match t.descr with None -> "--" | Some d -> d)

let pkgs_of_repo (t : resolved Repo.t) =
  List.map
    ~f:(fun (pkg : Opam.t) ->
      let name = pkg.name in
      let version = pkg.version in
      let loc = Repo.Url.to_string t.url in
      let pinned =
        match t.url with Git _ -> true | _ -> guess_pin ~version ~loc
      in
      let descr = None in
      { name; version; descr; pinned; loc })
    t.provided_packages

let pkgs_of_duniverse t =
  let pkgs = List.map ~f:pkgs_of_repo t in
  let pkgs = List.flatten pkgs in
  List.sort ~cmp:compare_pkg pkgs

let with_descr pkgs =
  OpamGlobalState.with_ `Lock_none (fun global_state ->
      OpamSwitchState.with_ `Lock_none global_state (fun switch_state ->
          List.map
            ~f:(fun pkg ->
              let opam = OpamPackage.create pkg.name pkg.version in
              match OpamSwitchState.opam switch_state opam with
              | opam -> { pkg with descr = OpamFile.OPAM.synopsis opam }
              | exception Not_found ->
                  (* This pkg.version doesn't exist in the switch and therefore
                     comes from a pin *)
                  { pkg with pinned = true })
            pkgs))

let run (`Root root) (`Lockfile explicit_lockfile) short () =
  let open Result.O in
  Common.find_lockfile ~explicit_lockfile ~quiet:short root >>= fun lockfile ->
  Lockfile.to_duniverse lockfile >>| fun duniverse ->
  let pkgs = pkgs_of_duniverse duniverse in
  let pkgs = with_descr pkgs in
  let max_name, max_version =
    List.fold_left
      ~f:(fun (max_name, max_version) t ->
        ( max (String.length (OpamPackage.Name.to_string t.name)) max_name,
          max
            (String.length (OpamPackage.Version.to_string t.version))
            max_version ))
      ~init:(0, 0) pkgs
  in
  let pp = pp ~max_name ~max_version ~short in
  if not short then
    Common.Logs.app (fun l ->
        let duniverse = Fpath.(v (Sys.getcwd ()) // Config.vendor_dir / "") in
        l "The vendor directory is %a" Pp.Styled.path duniverse);
  List.iter ~f:(fun pkg -> Fmt.pr "%a\n" pp pkg) pkgs

open Cmdliner

let short =
  let doc =
    Arg.info ~doc:"Don't print a header, and only display package names."
      [ "short"; "s" ]
  in
  Arg.(value & flag doc)

let info =
  let exits = Term.default_exits in
  let doc = Fmt.str "Display the list of monorepo packages" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "List the opam packages present in the lockfile that must be installed \
         in source mode in the duniverse.";
      `P
        "Unless the --short switch is used, the output format displays one \
         package per line, and each line contains the name of the package, the \
         installed version and a source location. In color mode, pinned \
         packages and packages defined in overlays have a blue version.";
    ]
  in
  Term.info "list" ~doc ~exits ~man

let term =
  let open Term in
  term_result
    (const run $ Common.Arg.root $ Common.Arg.lockfile $ short
   $ Common.Arg.setup_logs ())

let cmd = (term, info)
