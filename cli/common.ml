module Ffmt = Fmt
open Stdune
open Duniverse_lib

module Arg = struct
  let named f = Cmdliner.Term.(app (const f))

  let fpath = Cmdliner.Arg.conv ~docv:"PATH" (Fpath.of_string, Fpath.pp)

  let repo =
    let doc = "Path to Git repository to store vendored code in." in
    named
      (fun x -> `Repo x)
      Cmdliner.Arg.(value & opt fpath (Fpath.v ".") & info [ "r"; "repo" ] ~docv:"TARGET_REPO" ~doc)

  let yes =
    let doc = "Do not prompt for confirmation and always assume yes" in
    named (fun x -> `Yes x) Cmdliner.Arg.(value & flag & info [ "y"; "yes" ] ~doc)

  let non_empty_list_opt = Cmdliner.Term.pure (function [] -> None | l -> Some l)

  let duniverse_repos =
    let open Cmdliner in
    let docv = "REPOSITORIES" in
    let doc =
      "The list of $(docv) from your duniverse to process. If none is provided, all will be \
       processed."
    in
    named
      (fun x -> `Duniverse_repos x)
      Term.(non_empty_list_opt $ Arg.(value & pos_all string [] & info ~doc ~docv []))

  let thread_safe_reporter reporter =
    let lock = Mutex.create () in
    let { Logs.report } = reporter in
    let oui src level ~over k msgf =
      Mutex.lock lock;
      let x = report src level ~over k msgf in
      Mutex.unlock lock;
      x
    in
    Logs.{ report = oui }

  let setup_logs () =
    Printexc.record_backtrace true;
    let setup_log style_renderer level =
      Fmt_tty.setup_std_outputs ?style_renderer ();
      Logs.set_level level;
      Logs.set_reporter (thread_safe_reporter (Logs_fmt.reporter ()))
    in
    let global_option_section = "COMMON OPTIONS" in
    let open Cmdliner.Term in
    const setup_log
    $ Fmt_cli.style_renderer ~docs:global_option_section ()
    $ Logs_cli.level ~docs:global_option_section ()
end

module Logs = struct
  let app ?src f =
    Logs.app ?src (fun l ->
        f (fun ?header ?tags fmt -> l ?header ?tags ("%a" ^^ fmt) Duniverse_lib.Styled_pp.header ()))
end

(** Checks that all provided repos match a source repo in the duniverse *)
let check_duniverse_repos duniverse_repos src_deps =
  let dir_names = List.map ~f:(fun (s : _ Duniverse.Deps.Source.t) -> s.dir) src_deps in
  let found = List.sort_uniq ~compare:String.compare dir_names in
  let asked = List.sort_uniq ~compare:String.compare duniverse_repos in
  let rec diff acc asked_l found_l =
    match (asked_l, found_l) with
    | asked :: asked_tl, found :: found_tl ->
        if String.equal asked found then diff acc asked_tl found_tl
        else diff (asked :: acc) asked_tl found_l
    | asked :: asked_tl, [] -> diff (asked :: acc) asked_tl found_l
    | [], [] -> List.rev acc
    | [], _ -> assert false
  in
  match diff [] asked found with
  | [] -> Ok ()
  | unmatched ->
      let sep fmt () = Ffmt.pf fmt " " in
      Rresult.R.error_msgf "The following repos are not in your duniverse: %a"
        Ffmt.(list ~sep string)
        unmatched

let should_consider ~to_consider (src_dep : _ Duniverse.Deps.Source.t) =
  List.mem ~set:to_consider src_dep.dir

(** Filters the duniverse according to the CLI provided list of repos *)
let filter_duniverse ~to_consider src_deps =
  let open Rresult in
  match to_consider with
  | None -> Ok src_deps
  | Some to_consider ->
      let filtered = List.filter ~f:(should_consider ~to_consider) src_deps in
      check_duniverse_repos to_consider filtered >>= fun () -> Ok filtered
