let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    assert false
  with End_of_file ->
    close_in chan;
    List.rev !lines

let pp_string_list =
  Fmt.(list ~sep:(const string " ") string)

let pp_options fmt = function
  | [] -> ()
  | options ->
    pp_string_list fmt options;
    Fmt.pf fmt " "

let read_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.sort String.compare

let get_dirs dir =
  let is_dir f = Filename.concat dir f |> Sys.is_directory in
  read_dir dir |> List.filter is_dir

let get_files dir =
  let is_file f = not (Filename.concat dir f |> Sys.is_directory) in
  read_dir dir |> List.filter is_file

let cwd_options_file = "test-case.opts"
let cwd_test_file_md = "test-case.md"
let cwd_test_file_t = "test-case.t"

type dir =
  { test_file : string
  ; target_file : string
  ; expected_file : string
  ; options : string list
  ; dir_name : string
  }

let test_file ~dir_name files =
  let is_test_file f = String.equal f cwd_test_file_md || String.equal f cwd_test_file_t in
  match List.filter is_test_file files with
  | [test_file] -> test_file
  | [] ->
    Printf.eprintf "No test file for %s\n" dir_name;
    Printf.eprintf "There should be one of %s or %s\n" cwd_test_file_md cwd_test_file_t;
    exit 1
  | _ ->
    Printf.eprintf "More than one test file for %s\n" dir_name;
    Printf.eprintf "There should be only one of %s or %s\n" cwd_test_file_md cwd_test_file_t;
    exit 1

let expected_file ~dir_name ~test_file files =
  let is_expected_file f = String.equal (Filename.extension f) ".expected" in
  match List.filter is_expected_file files with
  | [] -> test_file
  | [expected_file] -> expected_file
  | _ ->
    Printf.eprintf "More than one .expected file for %s\n" dir_name;
    exit 1

let dir dir_name =
  let files = get_files dir_name in
  let test_file = test_file ~dir_name files in
  let expected_file = expected_file ~dir_name ~test_file files in
  let target_file = dir_name ^ ".actual" in
  let options_file = Filename.concat dir_name cwd_options_file in
  let options_file_exists = Sys.file_exists options_file in
  let options = if options_file_exists then read_file options_file else [] in
  { test_file
  ; target_file
  ; expected_file
  ; options
  ; dir_name
  }

let pr_runtest_alias dir =
  Fmt.pr
    {|
(alias
 (name runtest)
 (action (diff %s/%s %s)))
|}
    dir.dir_name
    dir.expected_file
    dir.target_file

let pr_rule ~pp_action dir =
  Fmt.pr
    {|
(rule
 (target %s)
 (deps (package mdx) (source_tree %s))
 (action%a))
|}
    dir.target_file
    dir.dir_name
    pp_action dir

type generator =
  { pp_expect_action : Format.formatter -> dir -> unit
  ; pp_failure_action : Format.formatter -> dir -> unit
  }

let pr_rule ~pp_action dir_name =
  let dir = dir dir_name in
  pr_rule ~pp_action dir;
  pr_runtest_alias dir

let rule_gen generator rule_type () =
  let pp_action =
    match rule_type with
    | `Test_expect -> generator.pp_expect_action
    | `Test_failure -> generator.pp_failure_action
  in
  List.iter (pr_rule ~pp_action) (get_dirs ".")

let run generator =
  let open Cmdliner in
  let cmds =
    Term.[
      const (rule_gen generator `Test_expect) $ const (), info "test_expect";
      const (rule_gen generator `Test_failure) $ const (), info "test_failure";
    ]
  in
  let default =
    let doc = "Generate dune files for the binary tests." in
    let exits = Term.default_exits in
    let man = [] in
    Term.(
      ret (const (`Help (`Auto, None))),
      info "gen_dune_rules" ~doc ~exits ~man
    )
  in
  Term.exit (Term.eval_choice default cmds)