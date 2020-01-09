module Code_block = struct
  type t =
    { location : Odoc_model.Location_.span
    ; contents : string
    }
end

(* drop_first_and_last [1; 2; 3; 4] = [2; 3]. *)
let drop_first_and_last list = List.tl list |> List.rev |> List.tl |> List.rev

let slice
      file_contents
      ~(start : Odoc_model.Location_.point)
      ~(end_ : Odoc_model.Location_.point)
  =
  let lines_to_include =
    String.split_on_char '\n' file_contents
    |> List.mapi (fun i line -> succ i, line)
    |> List.filter (fun (lnum, _) -> start.line <= lnum && lnum <= end_.line)
    |> List.map snd
  in
  match lines_to_include with
  | [] -> ""
  | [ line ] -> String.sub line start.column (end_.column - start.column)
  | list ->
    (* Imagine we were slicing the file from (Line 2, Column 3) to (Line 6, Column 7):

       0123456789
       1 ----------
       2 ---[---
       3 ---------
       4 --
       5 ----------
       6 -------]--
       7 ----------
       8 ----------

       The code below handles this multiline case, concatenating the included substrings
       from lines 2-6 ([lines_to_include]). *)
    let first_line = List.hd list in
    let last_line = List.rev list |> List.hd in
    let first_line =
      String.sub first_line start.column (String.length first_line - start.column)
    in
    let last_line = String.sub last_line 0 end_.column in
    String.concat "\n" ([ first_line ] @ drop_first_and_last list @ [ last_line ])
;;

(* Imagine a docstring that is within a file with four characters # of indentation. (I'll
   use square brackets rather than parens to avoid escaping):

   ####[** foo
   ####
   ####bar
   ####
   ####baz *]
   ####val x : int
   ####val y : int

   According to odoc, the "b" in "bar" is at column 0 inside the docstring and at column 4
   within the broader file. That is correct. But it says the "f" in "foo" is at column 1
   inside the docstring and column 5 within the file. This isn't right.

   The problem is that it starts counting the inside-the-docstring column number from the
   end of "[**", but doesn't add those three characters to the within-the-file column
   number. Here, we make the adjustment.
*)
let account_for_docstring_open_token (location : Odoc_model.Location_.span) =
  let shift = 3 in
  { location with
    start = { location.start with column = location.start.column + shift }
  ; end_ = { location.end_ with column = location.end_.column + shift }
  }
;;

let extract_code_blocks ~(location : Lexing.position) ~docstring =
  let rec acc blocks =
    List.map
      (fun block ->
         match Odoc_model.Location_.value block with
         | `Code_block contents ->
           let location =
             if location.pos_lnum = block.location.start.line
             then account_for_docstring_open_token block.location
             else block.location
           in
           [ { Code_block.location; contents } ]
         | `List (_, _, lists) -> List.map acc lists |> List.concat
         | _ -> [])
      blocks
    |> List.concat
  in
  let parsed = Odoc_parser.parse_comment_raw ~location ~text:docstring in
  List.iter (fun error -> failwith (Odoc_model.Error.to_string error)) parsed.warnings;
  List.map
    (fun element ->
       match Odoc_model.Location_.value element with
       | #Odoc_parser.Ast.nestable_block_element as e ->
         acc [ { Odoc_model.Location_.location = element.location; value = e } ]
       | `Tag tag ->
         (match tag with
          | `Deprecated blocks -> acc blocks
          | `Param (_, blocks) -> acc blocks
          | `Raise (_, blocks) -> acc blocks
          | `Return blocks -> acc blocks
          | `See (_, _, blocks) -> acc blocks
          | `Before (_, blocks) -> acc blocks
          | _ -> [])
       | `Heading _ -> [])
    parsed.value
  |> List.concat
;;

let docstrings lexbuf =
  let rec loop list =
    match Ppxlib_ast.Lexer.token_with_comments lexbuf with
    | Ppxlib_ast.Parser.EOF -> list
    | Ppxlib_ast.Parser.DOCSTRING docstring ->
      let docstring =
        ( Ocaml_common.Docstrings.docstring_body docstring
        , Ocaml_common.Docstrings.docstring_loc docstring )
      in
      loop (docstring :: list)
    | _ -> loop list
  in
  loop [] |> List.rev
;;

let docstring_code_blocks str =
  Ppxlib_ast.Lexer.handle_docstrings := true;
  Ppxlib_ast.Lexer.init ();
  List.map
    (fun (docstring, (location : Warnings.loc)) ->
       extract_code_blocks ~location:location.loc_start ~docstring)
    (docstrings (Lexing.from_string str))
  |> List.concat
;;
