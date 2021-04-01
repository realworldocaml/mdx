let keep_asm_files_load ppf program =
  let open Config in
  let open Opttoploop in
  let dll =
    if !Clflags.keep_asm_file then !phrase_name ^ ext_dll
    else Filename.temp_file ("caml" ^ !phrase_name) ext_dll
  in
  let filename = Filename.chop_extension dll in
  let middle_end =
    if Config.flambda then Flambda_middle_end.lambda_to_clambda
    else Closure_middle_end.lambda_to_clambda
  in
  Printf.printf "filename: %s\n%!" filename;
  Printf.printf "obj: %s\n%!" (filename ^ ext_obj);
  Asmgen.compile_implementation ~toplevel:need_symbol ~backend ~filename
    ~prefixname:filename ~middle_end ~ppf_dump:ppf program;
  Asmlink.call_linker_shared [ filename ^ ext_obj ] dll;
  if not !Clflags.keep_asm_file then Sys.remove (filename ^ ext_obj);
  let dll =
    if Filename.is_implicit dll then Filename.concat (Sys.getcwd ()) dll
    else dll
  in
  Printf.printf "dll: %s\n%!" dll;
  let res = dll_run dll !phrase_name in
  (if not !Clflags.keep_asm_file then
   try Sys.remove dll with Sys_error _ -> ());
  (* note: under windows, cannot remove a loaded dll
     (should remember the handles, close them in at_exit, and then remove
     files) *)
  res

let () =
  Clflags.native_code := true;
  Opttoploop.set_paths ();
  Opttoploop.initialize_toplevel_env ();
  Opttoploop.register_jit
    { load = keep_asm_files_load; lookup_symbol = Opttoploop.default_lookup };
  Clitop.main ~name:"nattop" ~eval_phrase:Opttoploop.execute_phrase
    ~loop:Opttoploop.loop ()
