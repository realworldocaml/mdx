open Import
open Duniverse

type t = {
  name : string;
  version : string;
  loc : string;
  pinned : bool;
  descr : string option;
}

let pad s n =
  let len = String.length s in
  if len >= n then "" else String.make (n - len) ' '

(* FIXME: replace this once we remember the remotes used during lock. *)
let guess_pin ~version ~loc =
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

let compare_pkg x y = String.compare x.name y.name

let pp ~max_name ~max_version ~short ppf t =
  if short then Fmt.string ppf t.name
  else if t.pinned then
    let p_name = pad t.name max_name in
    let p_version = pad t.version max_version in
    Fmt.pf ppf "%a%s %a%s %a" pp_name t.name p_name pp_pin_version t.version
      p_version pp_pin_loc t.loc
  else
    let p_name = pad t.name max_name in
    let p_version = pad t.version max_version in
    Fmt.pf ppf "%a%s %a%s %s" pp_name t.name p_name pp_version t.version
      p_version
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
  (* FIXME: why do we have duplicates here ? *)
  List.sort_uniq ~cmp:compare_pkg pkgs

let with_descr pkgs =
  OpamGlobalState.with_ `Lock_none (fun global_state ->
      OpamSwitchState.with_ `Lock_none global_state (fun switch_state ->
          List.map
            ~f:(fun pkg ->
              let opam =
                OpamPackage.create
                  (OpamPackage.Name.of_string pkg.name)
                  (OpamPackage.Version.of_string pkg.version)
              in
              try
                let opam = OpamSwitchState.opam switch_state opam in
                let descr = OpamFile.OPAM.synopsis opam in
                { pkg with descr }
              with Not_found -> { pkg with pinned = true })
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
        ( max (String.length t.name) max_name,
          max (String.length t.version) max_version ))
      ~init:(0, 0) pkgs
  in
  let pp = pp ~max_name ~max_version ~short in
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
      `P "List selections of opam packages available in the monorepo.";
      `P
        "Unless the --short switch is used, the output format displays one \
         package per line, and each line contains the name of the package, the \
         installed version and a source location. In color mode, root packages \
         (as opposed to automatically installed ones because of dependencies) \
         are underlined.";
    ]
  in
  Term.info "list" ~doc ~exits ~man

let term =
  let open Term in
  term_result
    (const run $ Common.Arg.root $ Common.Arg.lockfile $ short
   $ Common.Arg.setup_logs ())

let cmd = (term, info)
