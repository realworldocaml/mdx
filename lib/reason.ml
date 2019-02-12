(** Taken from reason_type_of_ocaml_type.ml *)

let () = Reason_pprint_ast.configure
  (* This can be made pluggable in the future. *)
  ~width:80
  ~assumeExplicitArity:true
  ~constructorLists:[]

let reasonFormatter = Reason_pprint_ast.createFormatter ()

(* int list *)
let parseAsCoreType str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.core_type
  |> reasonFormatter#core_type formatter

(* type a = int list *)
let parseAsImplementation str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.implementation
  |> reasonFormatter#structure [] formatter

(* val a: int list *)
let parseAsInterface str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.interface
  |> reasonFormatter#signature [] formatter

(* sig val a: int list end *)
(* This one is a hack; we should have our own parser entry point to module_type.
But that'd require modifying compiler-libs, which we'll refrain from doing. *)
let parseAsCoreModuleType str formatter =
  Lexing.from_string ("module X: " ^ str)
  |> Reason_toolchain.ML.interface
  |> reasonFormatter#signature [] formatter

let parseAsToplevelPhrase str formatter =
  Lexing.from_string str
  |> Reason_toolchain.ML.toplevel_phrase
  |> reasonFormatter#toplevel_phrase formatter

(* Quirky merlin/ocaml output that doesn't really parse. *)
let parseAsWeirdListSyntax str a =
  if str = "type 'a list = [] | :: of 'a * 'a list" then "type list 'a = [] | :: of list 'a 'a"
  (* Manually creating an error is tedious, so we'll put a hack here to throw the previous error. *)
  else raise (Syntaxerr.Error a)

let reason_of_ocaml str =
  let formatter = Format.str_formatter in
  let output = try ( parseAsCoreType str formatter; Some (Format.flush_str_formatter ()))
  with Syntaxerr.Error _ ->
  try ( parseAsInterface str formatter; Some (Format.flush_str_formatter ()))
  with Syntaxerr.Error _ ->
  try ( parseAsImplementation str formatter; Some (Format.flush_str_formatter ()))
  with Syntaxerr.Error _ ->
  try ( parseAsCoreModuleType str formatter; Some (Format.flush_str_formatter ()))
  with Syntaxerr.Error _ ->
  try ( parseAsToplevelPhrase str formatter; Some (Format.flush_str_formatter ()))
  with Syntaxerr.Error a ->
  try (Some (parseAsWeirdListSyntax str a))
  with _ -> None in
  match output with
  | Some str when str == "(* top directives not supported *)" -> raise (Failure str)
  | x -> x

let maybe_split =
  function
  | None  -> None
  | Some str -> Some (Compat.String.split_on_char '\n' str)

let reason_of_lines lines =
  lines
  |> String.concat "\n"
  |> reason_of_ocaml
  |> maybe_split
