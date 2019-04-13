(** Taken from reason_type_of_ocaml_type.ml *)
let src = Logs.Src.create "reason.pp"
module Log = (val Logs.src_log src : Logs.LOG)

let () = Reason_pprint_ast.configure
  (* This can be made pluggable in the future. *)
  ~width:80
  ~assumeExplicitArity:true
  ~constructorLists:[]

let maybe_split =
  function
  | None  -> None
  | Some str -> Some (Compat.String.split_on_char '\n' str)

let from_lines_to_lines lines transformer =
  lines
  |> String.concat "\n"
  |> transformer
  |> maybe_split

let dump_list ppf s = Fmt.pf ppf "%S" (String.concat "\n" s)

let dump ppf output =
  Fmt.pf ppf "%a" Fmt.(Dump.option dump_list) output

module Make_syntax_printer(Parser : Reason_toolchain.Toolchain)(Printer : Reason_toolchain.Toolchain) = struct
  let parseAsImplementation str formatter =
    Lexing.from_string str
    |> Parser.implementation_with_comments
    |> Printer.print_implementation_with_comments formatter

  let parseAsInterface str formatter =
    Lexing.from_string str
    |> Parser.interface_with_comments
    |> Printer.print_interface_with_comments formatter
  
  let try_transform str =
    let formatter = Format.str_formatter in
    let output = try ( parseAsInterface str formatter; Some (Format.flush_str_formatter ()))
    with Syntaxerr.Error _ ->
    try ( parseAsImplementation str formatter; Some (Format.flush_str_formatter ()))
    with _ -> None in
    match output with
    | Some str when str == "(* top directives not supported *)" -> raise (Failure str)
    | x -> x
  
  let transform lines =
    let output = from_lines_to_lines lines try_transform in
    Log.debug (fun l -> l "tranform: %a" dump output);
    output
end

module FromOCamlToReason = Make_syntax_printer(Reason_toolchain.ML)(Reason_toolchain.RE)

module FromReasonToOCaml = Make_syntax_printer(Reason_toolchain.RE)(Reason_toolchain.ML)

(* 
module Reason = struct

  let parseAsCoreType str formatter =
    Lexing.from_string str
    |> Reason_toolchain.ML.core_type
    |> reasonFormatter#core_type formatter

  let parseAsImplementation str formatter =
    Lexing.from_string str
    |> Reason_toolchain.ML.implementation
    |> reasonFormatter#structure [] formatter

  let parseAsInterface str formatter =
    Lexing.from_string str
    |> Reason_toolchain.ML.interface
    |> reasonFormatter#signature [] formatter

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
  

  let reason_of_lines lines =
    lines
    |> String.concat "\n"
    |> reason_of_ocaml
    |> maybe_split
end *)
(* 
let reason_from_ocaml lines =
  from_lines_to_lines lines (fun str ->
    let ast = Lexing.from_string str |> Reason_toolchain.ML.implementation_with_comments in
    Reason_toolchain.RE.print_implementation_with_comments Format.str_formatter ast;
    Format.flush_str_formatter ()
  )

let ocaml_from_reason lines =
  from_lines_to_lines lines (fun str ->
    let ast = Lexing.from_string str |> Reason_toolchain.RE.implementation_with_comments in
    Reason_toolchain.ML.print_implementation_with_comments Format.str_formatter ast;
    Format.flush_str_formatter ()
  )
   *)