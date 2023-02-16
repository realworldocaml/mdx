(*
 * Copyright (c) 2020 Ulysse Gérard <ulysse@tarides.com>
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

let run (`Setup ()) (`Prelude prelude) (`Directories dirs) =
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  let line fmt = Fmt.kpf (fun ppf -> Fmt.string ppf "\n") ppf fmt in
  let pp_list ppf =
    Fmt.pf ppf "[%a]\n" (Fmt.list ~sep:(Fmt.any "; ") Fmt.Dump.string)
  in

  let pp_ocaml_env ppf = function
    | Mdx.Ocaml_env.Default -> Fmt.string ppf "Mdx.Ocaml_env.Default"
    | Mdx.Ocaml_env.User_defined s ->
        Fmt.pf ppf "(Mdx.Ocaml_env.User_defined %S)" s
  in

  let pp_env ppf = function
    | `All -> Fmt.string ppf "`All"
    | `One ocaml_env -> Fmt.pf ppf "(`One %a)" pp_ocaml_env ocaml_env
  in

  let pp_prelude ppf (env, filename) =
    Fmt.pf ppf "(%a, %S)" pp_env env filename
  in

  let pp_preludes ppf preludes =
    Fmt.pf ppf "[%a]\n" (Fmt.list ~sep:(Fmt.any "; ") pp_prelude) preludes
  in

  line "let run_exn_defaults =";
  line "  let open Mdx_test in";
  line "  let packages =";
  line "    Package.[";
  line "      unix;";
  line "      findlib_top;";
  line "      findlib_internal;";
  line "      compilerlibs_toplevel;";
  line "    ]";
  line "  in";
  line "  let predicates = Predicate.[ byte; toploop ] in";
  line "  let non_deterministic =";
  line "    match Sys.getenv_opt \"MDX_RUN_NON_DETERMINISTIC\" with";
  line "    | Some _ -> true";
  line "    | None -> false";
  line "  in";
  line "  run_exn ~packages ~predicates ~prelude_str:[]";
  line "    ~non_deterministic";
  line "    ~silent_eval:false ~record_backtrace:false";
  line "    ~syntax:None ~silent:false";
  line "    ~verbose_findlib:false ~section:None";
  line "    ~root:None ~force_output:false";
  line "    ~output:(Some `Stdout)";

  line "let file = Sys.argv.(1)";
  line "let prelude = %a" pp_preludes prelude;
  line "let directives = List.map (fun path ->";
  line "  Mdx_top.Directory path) %a" pp_list dirs;
  line "let _ = run_exn_defaults";
  line "  ~file";
  line "  ~prelude";
  line "  ~directives";
  Fmt.flush ppf ();
  Buffer.output_buffer stdout buffer;
  0

let term = Cmdliner.Term.(const run $ Cli.setup $ Cli.prelude $ Cli.directories)

let doc =
  "Generate the source for a specialized testing binary. This command is meant \
   to be used by dune only. There are no stability guarantees."

let info = Cmdliner.Cmd.info "dune-gen" ~doc
let cmd = Cmdliner.Cmd.v info term
