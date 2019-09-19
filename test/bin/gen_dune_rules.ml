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

let is_testcase filename =
  let valid_extensions = [".md"; ".t"] in
  List.exists (Filename.check_suffix filename) valid_extensions

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

(* Copied from Filename (stdlib) for pre-4.04 compatibility *)
let chop_extension name =
  let is_dir_sep s i = match Sys.os_type with
    | "Unix" -> s.[i] = '/'
    | "Win32" | "Cygwin" ->
      let c = s.[i] in
      c = '/' || c = '\\' || c = ':'
    | _ -> assert false
  in
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then invalid_arg "Filename.chop_extension"
    else if name.[i] = '.' then String.sub name 0 i
    else search_dot (i - 1) in
  search_dot (String.length name - 1)

let options_of_file file =
  let options_file = chop_extension file ^ ".opts" in
  if not (Sys.file_exists options_file) then
    [""]
  else
    read_file options_file

let pp_options =
  Fmt.(list ~sep:(const string " ") string)

(* Tests that the output of 'ocaml-mdx test [options] <file>' is equal to the
   contents of '<file>'. Uses the rule generator build into ocaml-mdx. *)
let test_fixpoint_rule file =
  let options = options_of_file file in
  let command =
    let args = file::options in
    Fmt.strf "ocaml-mdx rule %a" pp_options args
  in
  match Unix.system command with
  | WEXITED 0 -> ()
  | WEXITED i -> exit i
  | WSIGNALED _ | WSTOPPED _ -> exit 1

(* Tests that the output of 'ocaml-mdx test [options] <file>' is equal to the
   contents of '<file>.expected'. *)
let test_expect_rule file =
  let expected_file = file ^ ".expected" in
  if not (Sys.file_exists expected_file) then
    failwith (
      Fmt.strf "No %s file found when generating an expect rule" expected_file
    );
  let options = options_of_file file in
  Fmt.pr
    {|
(alias
  (name runtest)
  (deps (:x %s) (:y %s) (package mdx))
  (action
    (progn
      (run ocaml-mdx test --force-output %a %%{x})
      (diff? %%{y} %%{x}.corrected))
  )
)
|}
    file
    expected_file
    pp_options options

let rule_gen rule_type () =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter is_testcase
  |> List.iter (match rule_type with
      | `Test_fixpoint -> test_fixpoint_rule
      | `Test_expect -> test_expect_rule
    )

open Cmdliner

let cmds =
  Term.[
    const (rule_gen `Test_fixpoint) $ const (), info "test_fixpoint";
    const (rule_gen `Test_expect) $ const (), info "test_expect"
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

