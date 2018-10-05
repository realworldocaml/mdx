(*
 * Copyright (c) 2018 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Chunk = struct
  type kind = OCaml | Raw
  type response = (kind * string)

  type t =
    { ocaml_code : string;
      toplevel_responses : response list; }

  let v ~ocaml_code ~toplevel_responses = {ocaml_code; toplevel_responses}
  let code c = c.ocaml_code
end

module Part = struct
  type t =
    { name : string;
      chunks : Chunk.t list; }

  let v ~name ~chunks = { name; chunks }
  let name {name;_} = name
  let chunks {chunks;_} = chunks
end

module Document = struct
  type t =
    { parts : Part.t list; matched : bool; }

  let v ~parts ~matched = {parts; matched}
  let parts {parts;_} = parts
end

module Lexbuf = struct

  open Lexing

  type t = {
    contents: string;
    lexbuf  : lexbuf;
  }

  let toplevel_fname = "//toplevel//"

  let shift_toplevel_position ~start pos = {
    pos_fname = toplevel_fname;
    pos_lnum = pos.pos_lnum - start.pos_lnum + 1;
    pos_bol  = pos.pos_bol  - start.pos_cnum - 1;
    pos_cnum = pos.pos_cnum - start.pos_cnum - 1;
  }

  let shift_toplevel_location ~start loc =
    let open Location in
    {loc with loc_start = shift_toplevel_position ~start loc.loc_start;
              loc_end = shift_toplevel_position ~start loc.loc_end}

  let initial_pos = {
    pos_fname = toplevel_fname;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0;
  }

  let semisemi_action =
    let lexbuf = Lexing.from_string ";;" in
    match Lexer.token lexbuf with
    | Parser.SEMISEMI ->
      lexbuf.Lexing.lex_last_action
    | _ -> assert false

  let v ~fname contents =
    let lexbuf = Lexing.from_string contents in
    lexbuf.lex_curr_p <- {initial_pos with pos_fname = fname};
    Location.input_name := fname;
    { contents; lexbuf }

  let of_file fname =
    let ic = open_in fname in
    let len = in_channel_length ic in
    let result = really_input_string ic len in
    close_in_noerr ic;
    v ~fname result

  let shift_location_error start =
    let open Location in
    let rec aux (error : Location.error) =
      {error with sub = List.map aux error.sub;
                  loc = shift_toplevel_location ~start error.loc}
    in
    aux

end

module Phrase = struct

  open Lexing
  open Parsetree

  (** {1 Phrase parsing} *)

  type t = {
    startpos : position;
    endpos   : position;
    parsed   : (toplevel_phrase, exn) result;
  }

  let read lexbuf =
    let startpos = lexbuf.Lexing.lex_curr_p in
    let parsed = match Parse.toplevel_phrase lexbuf with
      | phrase -> Ok phrase
      | exception exn ->
        let exn = match Location.error_of_exn exn with
          | None -> raise exn
          | Some `Already_displayed -> raise exn
          | Some (`Ok error) ->
            Location.Error (Lexbuf.shift_location_error startpos error)
        in
        if lexbuf.Lexing.lex_last_action <> Lexbuf.semisemi_action then begin
          let rec aux () = match Lexer.token lexbuf with
            | Parser.SEMISEMI | Parser.EOF -> ()
            | _ -> aux ()
          in
          aux ();
        end;
        Error exn
    in
    let endpos = lexbuf.Lexing.lex_curr_p in
    { startpos; endpos; parsed }

  let read doc = match read doc.Lexbuf.lexbuf with
    | exception End_of_file -> None
    | t -> Some t

  (** *)

  type 'a kind =
    | Code of 'a
    | Expect of { location: Location.t;
                  responses: Chunk.response list;
                  nondeterministic: bool }
    | Part of { location: Location.t; name: string }

  exception Cannot_parse_payload of Location.t

  let string_of_location
      {Location.loc_start = {pos_fname; pos_lnum; pos_bol; pos_cnum};_}
    =
    Printf.sprintf "%s, line %d, col %d" pos_fname pos_lnum (pos_cnum - pos_bol)

  let payload_constants loc = function
    | PStr [{pstr_desc = Pstr_eval (expr, _); _}] ->
      let one {pexp_loc; pexp_desc; _} = match pexp_desc with
        | Pexp_apply ({pexp_desc = Pexp_ident ident; _},
                      [Asttypes.Nolabel, {pexp_desc = Pexp_constant const; _}]) ->
          (pexp_loc, Some ident, const)
        | Pexp_constant const -> (pexp_loc, None, const)
        | _ -> raise (Cannot_parse_payload pexp_loc)
      in
      let rec consts = function
        | {pexp_desc=Pexp_sequence(e, rest); _} -> one e :: consts rest
        | e -> [one e]
      in
      consts expr
    | PStr [] -> []
    | _ -> raise (Cannot_parse_payload loc)

  let payload_strings loc = function
    | PStr [] -> []
    | x ->
      let aux = function
        | _, Some {Location.txt = Longident.Lident "ocaml"; _},
          Pconst_string (str, _) -> (Chunk.OCaml, str)
        | _, None, Pconst_string (str, _) -> (Chunk.Raw, str)
        | loc, _, _ -> raise (Cannot_parse_payload loc)
      in
      List.map aux (payload_constants loc x)

  let attr_is x name = x.Asttypes.txt = name

  let kind phrase = match phrase.parsed with
    | Ok (Ptop_def [{pstr_desc = Pstr_extension((attr, payload), _attrs); pstr_loc}])
      when List.exists (attr_is attr) ["expect"; "expect.nondeterministic"] ->
      begin match payload_strings pstr_loc payload with
        | responses ->
          let nondeterministic = attr_is attr "expect.nondeterministic" in
          Expect { location = pstr_loc; responses; nondeterministic }
        | exception (Cannot_parse_payload loc) ->
          prerr_endline (string_of_location loc ^ ": cannot parse [%%expect] payload");
          Code ()
      end
    | Ok (Ptop_def [{pstr_desc = Pstr_attribute (name, payload); pstr_loc}])
      when name.Asttypes.txt = "part" ->
      begin match payload_strings pstr_loc payload with
        | [Chunk.Raw, part] -> Part { location = pstr_loc; name = part }
        | _ ->
          prerr_endline (string_of_location pstr_loc ^ ": cannot parse [@@@part] payload");
          Code ()
        | exception (Cannot_parse_payload loc) ->
          prerr_endline
            (string_of_location loc ^ ": cannot parse [@@@part] payload");
          Code ()
      end
    | _ -> Code ()

  (* Skip spaces as well as ';;' *)
  let skip_whitespace contents ?(stop=String.length contents) start =
    let rec loop start =
      if start >= stop then start else
        match contents.[start] with
        | ' ' | '\t' | '\n' -> loop (start + 1)
        | ';' when start + 1 < stop && contents.[start+1] = ';' ->
          loop (start + 2)
        | _ -> start
    in
    loop start

  let contents doc ?start ?stop phrase =
    let stop = match stop with
      | None -> phrase.endpos.pos_cnum
      | Some stop -> stop
    in
    let start = match start with
      | None -> phrase.startpos.pos_cnum
      | Some start -> start
    in
    let start = skip_whitespace doc.Lexbuf.contents ~stop start in
    String.sub doc.contents start (stop - start)

  let document doc ~matched phrases =
    let rec parts_of_phrase part acc = function
      | (_, Part { name; _ }) :: rest ->
        Part.v ~name:part ~chunks:(List.rev acc) ::
        parts_of_phrase name [] rest
      | (_, Expect _) :: rest ->
        parts_of_phrase part acc rest
      | (phrase, Code toplevel_responses) :: rest ->
        let ocaml_code = contents doc phrase in
        let chunk = Chunk.v ~ocaml_code ~toplevel_responses in
        parts_of_phrase part (chunk :: acc) rest
      | [] ->
        if part <> "" || acc <> [] then
          [Part.v ~name:part ~chunks:(List.rev acc)]
        else
          []
    in
    let parts = parts_of_phrase "" [] phrases in
    Document.v ~matched ~parts

  let dry_exec phrases =
    let rec aux acc = function
      | [] -> List.rev acc
      | (phrase, Code ()) :: rest ->
        begin match rest with
          | (_, Expect { responses; _ }) :: _ ->
            aux ((phrase, Code responses) :: acc) rest
          | _ -> aux ((phrase, Code []) :: acc) rest
        end
      | (_, (Part _ | Expect _) as phrase) :: rest ->
        aux (phrase :: acc) rest
    in
    aux [] phrases

  let read_all doc =
    let rec aux phrases = match read doc with
      | None        ->  List.rev phrases
      | Some phrase -> aux ((phrase, kind phrase) :: phrases)
    in
    dry_exec (aux [])

end

let find ~file ~part =
  let lexbuf = Lexbuf.of_file file in
  let v = Phrase.read_all lexbuf in
  let doc = Phrase.document lexbuf v ~matched:true in
  let parts = Document.parts doc in
  match part with
  | Some part ->
     (match List.find_opt (fun p -> String.equal (Part.name p) part) parts with
      | Some p ->
         Part.chunks p |> List.rev_map Chunk.code |> List.rev
      | None ->
         Fmt.failwith "Part %s not found in file %s" part file)
  | None ->
     List.fold_left (fun acc p ->
         let chunks = Part.chunks p |> List.rev_map Chunk.code in
         List.fold_left (fun acc x -> x :: acc) ("" :: acc) chunks
       ) [] parts |> List.rev

let replace ~file ~part ~lines =
  let lexbuf = Lexbuf.of_file file in
  let v = Phrase.read_all lexbuf in
  let doc = Phrase.document lexbuf v ~matched:true in
  let parts = Document.parts doc in
  let on_part p =
    let name = Part.name p in
    let lines =
      if String.equal name part then
        lines
      else
        Part.chunks p |> List.rev_map Chunk.code |> List.rev
    in
    if String.equal name "" then lines
    else ("[@@@part \"" ^ name ^ "\"];;") :: lines
  in
  List.map on_part parts
