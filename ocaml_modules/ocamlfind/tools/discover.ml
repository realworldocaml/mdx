module C = Configurator.V1

let () =
  C.main ~name:"findlib" (fun c ->
    let config_file = "findlib.conf.unused" in
    let ocaml_stdlib = C.ocaml_config_var_exn c "standard_library" in
    let ocaml_ldconf = Filename.concat ocaml_stdlib "ld.conf" in
    let has_autolinking = "true" in
    let system = C.ocaml_config_var_exn c "system" in
    let dll_suffix = C.ocaml_config_var_exn c "ext_dll" in
    Printf.printf {|
let config_file = %S;;
let ocaml_stdlib = %S;;
let ocaml_ldconf = %S;;
let ocaml_has_autolinking = %s;;
let libexec_name = "stublibs";;
let system = %S;;
let dll_suffix = %S;;
|} config_file ocaml_stdlib ocaml_ldconf has_autolinking system dll_suffix
  )
