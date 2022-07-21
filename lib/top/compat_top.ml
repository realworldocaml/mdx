let lookup_type typ env =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_type_by_name typ env |> fst
#else
  Env.lookup_type typ env
#endif

let lookup_value v env =
#if OCAML_VERSION >= (4, 10, 0)
  Env.find_value_by_name v env
#else
  Env.lookup_value v env
#endif

let find_value env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_value ~loc id env
#else
  Typetexp.find_value env loc id
#endif

let find_type env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_type ~loc id env
#else
  Typetexp.find_type env loc id
#endif

let find_constructor env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_constructor ~loc Env.Positive id env
#else
  Typetexp.find_constructor env loc id
#endif

let find_module env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_module ~loc id env
#else
  Typetexp.find_module env loc id
#endif

let find_modtype env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_modtype ~loc id env
#else
  Typetexp.find_modtype env loc id
#endif

let find_class env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_class ~loc id env
#else
  Typetexp.find_class env loc id
#endif

let find_class_type env loc id =
#if OCAML_VERSION >= (4, 10, 0)
  Env.lookup_cltype ~loc id env
#else
  Typetexp.find_class_type env loc id
#endif

let type_structure env str loc =
#if OCAML_VERSION >= (4, 14, 0)
  let tstr, _, _, _, env =
#else
  let tstr, _, _, env =
#endif
#if OCAML_VERSION >= (4, 12, 0)
    let _ = loc in
    Typemod.type_structure env str
#else
    Typemod.type_structure env str loc
#endif
  in
  tstr, env

let extension_constructor
    ~ext_type_path
    ~ext_type_params
    ~ext_args
    ~ext_ret_type
    ~ext_private
    ~ext_loc
    ~ext_attributes
  =
  let open Types in
  let ext_args =
    Cstr_tuple ext_args
  in
  { ext_type_path
  ; ext_type_params
  ; ext_args
  ; ext_ret_type
  ; ext_private
  ; ext_loc
  ; ext_attributes
#if OCAML_VERSION >= (4, 11, 0)
  ; ext_uid = Uid.mk ~current_unit:"mdx"
#endif
  }

let match_env
    ~value
    ~empty
    ~open_
    ~functor_arg
    ~constraints
    ~copy_types
    ~module_
    ~persistent
    ~type_
    ~modtype
    ~cltype
    ~class_
    ~extension
    ~value_unbound
    ~module_unbound
    env =
  ignore (constraints, persistent, copy_types, value_unbound, module_unbound);
  match env with
  | Env.Env_value (summary, id, _) ->
    value summary id
  | Env_empty -> empty ()
  | Env_open (summary, pid) ->
    open_ summary pid
  | Env_functor_arg (summary, id) -> functor_arg summary id
  | Env_module (summary, id, presence, _) ->
    let present = match presence with
      | Mp_present -> true
      | Mp_absent -> false
    in
    module_ summary id ~present
  | Env_type (summary, _, _) -> type_ summary
  | Env_modtype (summary, _, _) -> modtype summary
  | Env_cltype (summary, _, _) -> cltype summary
  | Env_class (summary, id, _) -> class_ summary id
  | Env_extension (summary, id, _) -> extension summary id
  | Env_constraints (summary, _) -> constraints summary
#if OCAML_VERSION >= (4, 10, 0)
  | Env_copy_types summary -> copy_types summary
  | Env_value_unbound (summary, _, _) -> value_unbound summary
  | Env_module_unbound (summary, _, _) -> module_unbound summary
#else
  | Env_copy_types (summary, _) -> copy_types summary
#endif
  | Env_persistent (summary, _) -> persistent summary

let ctype_is_equal =
#if OCAML_VERSION >= (4, 13, 0)
  Ctype.is_equal
#else
  Ctype.equal
#endif

let ctype_expand_head_and_get_desc env ty =
#if OCAML_VERSION >= (4, 14, 0)
  Types.get_desc (Ctype.expand_head env ty)
#else
  (Ctype.expand_head env ty).Types.desc
#endif

let ctype_get_desc ty =
#if OCAML_VERSION >= (4, 14, 0)
  Types.get_desc ty
#else
  (Ctype.repr ty).Types.desc
#endif

exception Exit_with_status of int


let execute_phrase print_outcome ppf phr =
#if OCAML_VERSION >= (4, 12, 0)
  match Toploop.execute_phrase print_outcome ppf phr with
  | v -> v
  | exception Compenv.Exit_with_status status ->
      raise (Exit_with_status status)
#else
  Toploop.execute_phrase print_outcome ppf phr
#endif

#if OCAML_VERSION < (4, 14, 0)
let std_err = Format.err_formatter

let patch_directive name directive =
  let patched_name = Format.asprintf "mdx_%s" name in
  let directive_info = Toploop.{ section = "MDX PATCHED"; doc = "Patched by MDX" } in
  Toploop.add_directive patched_name directive directive_info;
  patched_name

(* port of Topdirs.action_on_suberror *)
let action_on_suberror b =
  if not b && not !Sys.interactive then
    raise (Exit_with_status 125)

let dir_use ppf name =
  action_on_suberror (Toploop.use_file ppf name)

let mdx_use = patch_directive "use" (Directive_string (dir_use std_err))

let mdx_install_printer = patch_directive "install_printer" (Directive_ident (Topdirs.dir_install_printer std_err))
let mdx_remove_printer = patch_directive "remove_printer" (Directive_ident (Topdirs.dir_remove_printer std_err))
#endif

#if OCAML_VERSION >= (4, 11, 0) && OCAML_VERSION < (4, 14, 0)

let dir_use_output ppf name =
  action_on_suberror (Toploop.use_output ppf name)

let mdx_use_output = patch_directive "use_output" (Directive_string (dir_use_output std_err))
#endif

#if OCAML_VERSION < (4, 13, 0)
let mdx_trace = patch_directive "trace" (Directive_ident (Topdirs.dir_trace std_err))
let mdx_untrace = patch_directive "untrace" (Directive_ident (Topdirs.dir_untrace std_err))
let mdx_untrace_all = patch_directive "untrace_all" (Directive_none (Topdirs.dir_untrace_all std_err))
#endif

#if OCAML_VERSION < (4, 13, 0)
(* [load] cannot be patched to return errors because the underlying code is not exposed:
   It would require [Topdirs.load_file] with the first argument to be [false] but the exposed
   version hardcodes it to [true].
  *)
let mdx_load = patch_directive "load" (Directive_string (Topdirs.dir_load std_err))

(* On the other hand, [load_rec] can be patched because the curried [true] is the only
   difference between these directives *)
let dir_load_rec ppf name =
  action_on_suberror (Topdirs.load_file ppf name)

let mdx_load_rec = patch_directive "load_rec" (Directive_string (dir_load_rec std_err))

#elif OCAML_VERSION >= (4, 13, 0) && OCAML_VERSION < (4, 14, 0)
(* OCaml 4.13 exposes [Topeval.load_file] which allows us to patch [#load] too *)
let dir_load ppf name =
  action_on_suberror (Topeval.load_file false ppf name)

let mdx_load = patch_directive "load" (Directive_string (dir_load std_err))

(* This uses [Topeval.load_file] because [Topdirs.load_file] is deprecated on 4.13 *)
let dir_load_rec ppf name =
  action_on_suberror (Topeval.load_file true ppf name)

let mdx_load_rec = patch_directive "load_rec" (Directive_string (dir_load_rec std_err))
#endif

let redirect_directive directive =
  match directive with
#if OCAML_VERSION < (4, 14, 0)
  | "load" -> mdx_load
  | "load_rec" -> mdx_load_rec
  | "use" -> mdx_use
  | "install_printer" -> mdx_install_printer
  | "remove_printer" -> mdx_remove_printer
#endif
#if OCAML_VERSION >= (4, 11, 0) && OCAML_VERSION < (4, 14, 0)
  | "use_output" -> mdx_use_output
#endif
#if OCAML_VERSION < (4, 13, 0)
  | "trace" -> mdx_trace
  | "untrace" -> mdx_untrace
  | "untrace_all" -> mdx_untrace_all
#endif
  | v -> v
