module Code_block = struct
  type metadata = { language_tag : string; labels : string option }

  type t = {
    metadata : metadata option;
    delimiter : string option;
    content : Location.t; (* Location of the content *)
    code_block : Location.t; (* Location of the enclosing code block *)
  }
end

(* Parse and extract code block metadata from an odoc formatted docstring.

   Code blocks are the only thing we're interested in. This function parses
   the given text and extracts the metadata and enough location information
   from the code blocks be able to String.sub them out of the original text.

   [location] is the location of this docstring within the original file
   (ie, the location of the contents of the documentation comment). This is
   required so we can splice out the code blocks from the original file.

   The results are prepended in reverse order onto [acc]. *)
let extract_code_block_info acc ~(location : Lexing.position) ~docstring =
  let module O = Odoc_parser in
  let parsed = O.parse_comment ~location ~text:docstring in

  (* If odoc-parser produced any warnings, we raise them as errors here *)
  List.iter
    (fun error -> failwith (O.Warning.to_string error))
    (O.warnings parsed);

  (* Extract the useful info from what odoc has given us.

     Note, we don't use the contents of the code block that odoc has handed us
     as that has been stripped and we need all the relevant whitespace.
     Fortunately the location info give us enough info to be able to extract
     the code from the original text, whitespace and all.
  *)
  let handle_code_block : O.Loc.span -> _ -> Code_block.t =
    let convert_loc (sp : O.Loc.span) =
      Location.
        {
          loc_start = O.position_of_point parsed sp.start;
          loc_end = O.position_of_point parsed sp.end_;
          loc_ghost = false;
        }
    in
    fun location
        { O.Ast.meta; delimiter; content = { O.Loc.location = span; _ }; _ } ->
      let metadata =
        Option.map
          (fun { O.Ast.language; tags } ->
            let language_tag = O.Loc.value language in
            let labels = Option.map O.Loc.value tags in
            Code_block.{ language_tag; labels })
          meta
      in
      let content = convert_loc span in
      let code_block = convert_loc location in
      { metadata; delimiter; content; code_block }
  in

  (* Fold over the results from odoc-parser, recurse where necessary
     and extract the code block metadata *)
  let rec fold_fn acc (elt : O.Ast.block_element O.Loc.with_location) =
    match elt with
    | { O.Loc.value = `Code_block c; location } ->
        handle_code_block location c :: acc
    | { O.Loc.value = `List (_, _, lists); _ } ->
        List.fold_left (List.fold_left fold_fn) acc (lists :> O.Ast.t list)
    | { O.Loc.value = `Tag tag; _ } -> (
        match tag with
        | `Deprecated blocks
        | `Param (_, blocks)
        | `Raise (_, blocks)
        | `Return blocks
        | `See (_, _, blocks)
        | `Before (_, blocks) ->
            List.fold_left fold_fn acc (blocks :> O.Ast.t)
        | _ -> acc)
    | _ -> acc
  in

  List.fold_left fold_fn acc (O.ast parsed)

(* This function handles string containing ocaml code. It parses it as ocaml
   via compiler-libs, then for each odoc-formatted comment it then parses
   that via odoc-parser. The end result is a list of metadata about the code
   blocks within the comments. The result is given as an in-order list of
   [Code_block.t] values. *)
let docstring_code_blocks str =
  let initial_handle_docstrings = !Lexer.handle_docstrings in
  Fun.protect
    ~finally:(fun () -> Lexer.handle_docstrings := initial_handle_docstrings)
    (fun () ->
      Lexer.handle_docstrings := true;
      Lexer.init ();
      let lexbuf = Lexing.from_string str in
      let rec loop list =
        match Lexer.token_with_comments lexbuf with
        | Parser.EOF -> list
        | Parser.DOCSTRING docstring ->
            let body = Docstrings.docstring_body docstring in
            let loc = Docstrings.docstring_loc docstring in

            (* odoc-parser adjusts for the initial [** *)
            let adjustment = 3 (* String.length "(**" *) in

            let location =
              {
                loc.loc_start with
                pos_cnum = loc.loc_start.pos_cnum + adjustment;
              }
            in
            loop (extract_code_block_info list ~location ~docstring:body)
        | _ -> loop list
      in
      loop [] |> List.rev)

(* Given code block metadata and the original file, this function splices the
   contents of the code block from the original text and creates an Mdx
   Block.t, or reports the error (e.g., from invalid tags) *)
let make_block code_block file_contents =
  let handle_header = function
    | Some Code_block.{ language_tag; labels } ->
        let open Util.Result.Infix in
        let header = Block.Header.of_string language_tag in
        let* labels =
          match labels with
          | None -> Ok []
          | Some labels -> (
              match Label.of_string (String.trim labels) with
              | Ok labels -> Ok labels
              | Error msgs ->
                  Error (List.hd msgs) (* TODO: Report precise location *))
        in
        let language_label = Label.Language_tag language_tag in
        Ok (header, language_label :: labels)
    | None ->
        (* If not specified, blocks are run as ocaml blocks *)
        Ok (Some OCaml, [])
  in
  match handle_header code_block.Code_block.metadata with
  | Error _ as e -> e
  | Ok (header, labels) ->
      let slice (loc : Location.t) =
        let start = loc.loc_start.pos_cnum in
        let len = loc.loc_end.pos_cnum - start in
        String.sub file_contents start len
      in
      let delim = code_block.delimiter in
      let contents = slice code_block.content |> String.split_on_char '\n' in
      Block.mk ~loc:code_block.code_block ~section:None ~labels ~header
        ~contents ~legacy_labels:false ~errors:[] ~delim

(* Given the locations of the code blocks within [file_contents], then slice it up into
   [Text] and [Block] parts by using the starts and ends of those blocks as
   boundaries. *)
let extract_blocks code_blocks file_contents =
  let cursor, tokens =
    List.fold_left
      (fun (cursor, code_blocks) (code_block : Code_block.t) ->
        let pre_text =
          Document.Text
            (String.sub file_contents cursor
               (code_block.code_block.loc_start.pos_cnum - cursor))
        in
        let block =
          match make_block code_block file_contents with
          | Ok block -> Document.Block block
          | Error (`Msg msg) -> Fmt.failwith "Error creating block: %s" msg
        in
        (* append them in reverse order, since this is a fold_left *)
        let code_blocks = block :: pre_text :: code_blocks in
        (code_block.code_block.loc_end.pos_cnum, code_blocks))
      (0, []) code_blocks
  in
  let tokens = List.rev tokens in
  if cursor < String.length file_contents then
    let remainder =
      String.sub file_contents cursor (String.length file_contents - cursor)
    in
    if not (String.equal remainder "") then tokens @ [ Text remainder ]
    else tokens
  else tokens

let parse_mli file_contents =
  try
    let code_blocks = docstring_code_blocks file_contents in
    Ok (extract_blocks code_blocks file_contents)
  with exn -> Error [ `Msg (Printexc.to_string exn) ]

let parse_mld ?(filename = "_none_") file_contents =
  let location =
    Lexing.{ pos_bol = 0; pos_lnum = 1; pos_cnum = 0; pos_fname = filename }
  in
  let code_blocks =
    extract_code_block_info [] ~location ~docstring:file_contents |> List.rev
  in
  Ok (extract_blocks code_blocks file_contents)
