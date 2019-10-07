(*
 * Copyright (c) 2019 Craig Ferguson <me@craigfe.io>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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

let pp_deps = pp_string_list

let cwd_options_file = "test-case.opts"
let cwd_test_file = "test-case.md"
let cwd_expected_file = "test-case.md.expected"

type dir =
  { test_file : string
  ; target_file : string
  ; expected_file : string
  ; options : string list
  ; test_deps : string list
  }

let dir dir_name =
  let test_file = Filename.concat dir_name cwd_test_file in
  let expected_file =
    let file = Filename.concat dir_name cwd_expected_file in
    if Sys.file_exists file then file else test_file
  in
  let target_file = dir_name ^ ".corrected" in
  let options_file = Filename.concat dir_name cwd_options_file in
  let options_file_exists = Sys.file_exists options_file in
  let options = if options_file_exists then read_file options_file else [] in
  let test_deps =
    let base_deps = ["(package mdx)"; test_file] in
    if options_file_exists then base_deps @ [options_file] else base_deps
  in
  { test_file
  ; target_file
  ; expected_file
  ; options
  ; test_deps
  }

let pr_diff_rule dir =
  Fmt.pr
    {|
(alias
 (name runtest)
 (action (diff %s %s)))
|}
    dir.expected_file
    dir.target_file

(** Tests that the result of 'cd <dir_name> && ocaml-mdx test [options] test-case.md'
    is equal to '<dir_name>/test-case.md.expected' if it exists or to
    '<dir>/test-case.md' otherwise. *)
let test_expect_rule dir_name =
  let dir = dir dir_name in
  Fmt.pr
    {|
(rule
 (target %s)
 (deps (package mdx) (source_tree %s))
 (action
  (with-stdout-to %%{target}
   (chdir %s
    (run ocaml-mdx test --output - %a%s)))))
|}
    dir.target_file
    dir_name
    dir_name
    pp_options dir.options
    cwd_test_file;
  pr_diff_rule dir

(** Tests that 'ocaml-mdx test [options] <file>' exits with a failing code and
    that its output is equal to '<file>.expected'. *)
let test_failure_rule dir_name =
  let dir = dir dir_name in
  Fmt.pr
    {|
(rule
 (target %s)
 (deps %a)
 (action
  (with-outputs-to %%{target}
   (chdir %s
    (system "! ocaml-mdx test %a%s")))))
|}
    dir.target_file
    pp_deps dir.test_deps
    dir_name
    pp_options dir.options
    cwd_test_file;
  pr_diff_rule dir

let rule_gen rule_type () =
  let rule_generator = match rule_type with
    | `Test_expect -> test_expect_rule
    | `Test_failure -> test_failure_rule
  in
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter Sys.is_directory
  |> List.iter rule_generator

open Cmdliner

let cmds =
  Term.[
    const (rule_gen `Test_expect) $ const (), info "test_expect";
    const (rule_gen `Test_failure) $ const (), info "test_failure";
  ]

let default =
  let doc = "Generate dune files for the binary tests." in
  let exits = Term.default_exits in
  let man = [] in
  Term.(
    ret (const (`Help (`Auto, None))),
    info "gen_dune_rules" ~doc ~exits ~man
  )

let () = Term.exit (Term.eval_choice default cmds)
