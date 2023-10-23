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

open Util.Result.Infix

let loc_error ~loc fmt =
  Format.kasprintf
    (fun s -> Error (`Msg s))
    ("%a: invalid code block: " ^^ fmt)
    Stable_printer.Location.pp loc

let locate_error_msg ~loc s =
  Format.asprintf "%a: invalid code block: %s" Stable_printer.Location.pp loc s

let locate_errors ~loc r =
  Result.map_error
    (fun l -> List.map (fun (`Msg m) -> `Msg (locate_error_msg ~loc m)) l)
    r

module Header = struct
  type t = Shell of [ `Sh | `Bash ] | OCaml | Other of string

  let pp ppf = function
    | Shell `Sh -> Fmt.string ppf "sh"
    | Shell `Bash -> Fmt.string ppf "bash"
    | OCaml -> Fmt.string ppf "ocaml"
    | Other s -> Fmt.string ppf s

  let of_string = function
    | "" -> None
    | "sh" -> Some (Shell `Sh)
    | "bash" -> Some (Shell `Bash)
    | "ocaml" -> Some OCaml
    | s -> Some (Other s)

  let infer_from_file file =
    match Filename.(remove_extension (basename file), extension file) with
    | ("dune" | "dune-project"), _ -> Some (Other "scheme")
    | _, (".ml" | ".mli" | ".mlt" | ".eliom" | ".eliomi") -> Some OCaml
    | _, ".sh" -> Some (Shell `Sh)
    | _ -> None
end

type section = int * string

module Raw = struct
  type t =
    | Include of { loc : Location.t; section : section option; labels : string }
    | Any of {
        loc : Location.t;
        section : section option;
        header : string;
        contents : string list;
        label_cmt : string option;
        legacy_labels : string;
        errors : Output.t list;
      }

  let make ~loc ~section ~header ~contents ~label_cmt ~legacy_labels ~errors =
    Any { loc; section; header; contents; label_cmt; legacy_labels; errors }

  let make_include ~loc ~section ~labels = Include { loc; section; labels }
end

type cram_value = { language : [ `Sh | `Bash ]; non_det : Label.non_det option }

type ocaml_value = {
  env : Ocaml_env.t;
  non_det : Label.non_det option;
  errors : Output.t list;
  header : Header.t option;
}

type toplevel_value = { env : Ocaml_env.t; non_det : Label.non_det option }
type include_ocaml_file = { part_included : string option }
type include_other_file = { header : Header.t option }

type include_file_kind =
  | Fk_ocaml of include_ocaml_file
  | Fk_other of include_other_file

type include_value = { file_included : string; file_kind : include_file_kind }
type raw_value = { header : Header.t option }

type value =
  | Raw of raw_value
  | OCaml of ocaml_value
  | Cram of cram_value
  | Toplevel of toplevel_value
  | Include of include_value

type t = {
  loc : Location.t;
  section : section option;
  dir : string option;
  labels : Label.t list;
  legacy_labels : bool;
  contents : string list;
  skip : bool;
  version_enabled : bool;
  os_type_enabled : bool;
  set_variables : (string * string) list;
  unset_variables : string list;
  delim : string option;
  value : value;
}

let dump_section = Fmt.(Dump.pair int string)

let header t =
  match t.value with
  | Raw { header; _ } -> header
  | OCaml { header; _ } -> header
  | Cram { language; _ } -> Some (Header.Shell language)
  | Toplevel _ -> Some Header.OCaml
  | Include { file_kind = Fk_ocaml _; _ } -> Some Header.OCaml
  | Include { file_kind = Fk_other b; _ } -> b.header

let dump_value ppf = function
  | Raw _ -> Fmt.string ppf "Raw"
  | OCaml _ -> Fmt.string ppf "OCaml"
  | Cram _ -> Fmt.string ppf "Cram"
  | Toplevel _ -> Fmt.string ppf "Toplevel"
  | Include _ -> Fmt.string ppf "Include"

let dump ppf ({ loc; section; labels; contents; value; _ } as b) =
  Fmt.pf ppf
    "{@[loc: %a;@ section: %a;@ labels: %a;@ header: %a;@ contents: %a;@ \
     value: %a@]}"
    Stable_printer.Location.pp loc
    Fmt.(Dump.option dump_section)
    section
    Fmt.Dump.(list Label.pp)
    labels
    Fmt.(Dump.option Header.pp)
    (header b)
    Fmt.Dump.(list string)
    contents dump_value value

let pp_contents ?syntax:_ ppf t =
  Fmt.(list ~sep:(any "\n") string) ppf t.contents

let rec error_padding = function
  | [] -> []
  | [ (`Output _ as o); `Output padding ] when Util.String.all_blank padding ->
      [ o ]
  | x :: xs ->
      let xs = error_padding xs in
      x :: xs

let compute_delimiter ~base_delim outputs =
  let s =
    Format.asprintf "%a" (Format.pp_print_list (Output.pp ~pad:0)) outputs
  in
  let is_inadequate delim =
    Astring.String.is_infix ~affix:("]" ^ delim ^ "}") s
  in
  let rec loop n =
    let delim =
      match n with 0 -> base_delim | n -> Format.sprintf "%s_%d" base_delim n
    in
    if is_inadequate delim then loop (n + 1) else delim
  in
  loop 0

let pp_error ?syntax ?delim ppf outputs =
  match syntax with
  | Some Syntax.Markdown ->
      Fmt.pf ppf "```\n```mdx-error\n%a\n"
        Fmt.(list ~sep:(any "\n") Output.pp)
        outputs
  | Some Syntax.Mli | Some Syntax.Mld ->
      let err_delim = compute_delimiter ~base_delim:"err" outputs in
      Fmt.pf ppf "]%a[\n{%s@mdx-error[\n%a\n]%s}"
        Fmt.(option string)
        delim err_delim
        Fmt.(list ~sep:(any "\n") Output.pp)
        outputs err_delim
  | _ -> ()

let has_output t =
  match t.value with
  | OCaml { errors = []; _ } -> false
  | OCaml { errors = _; _ } -> true
  | _ -> false

let pp_value ?syntax ppf t =
  let delim = t.delim in
  match t.value with
  | OCaml { errors = []; _ } -> ()
  | OCaml { errors; _ } ->
      let errors = error_padding errors in
      pp_error ?syntax ?delim ppf errors
  | _ -> ()

let pp_footer ?syntax ppf t =
  let delim =
    if has_output t then (
      pp_value ?syntax ppf t;
      None)
    else t.delim
  in
  match syntax with
  | Some Syntax.Mli | Some Syntax.Mld ->
      Fmt.pf ppf "]%a}" Fmt.(option string) delim
  | Some Syntax.Cram -> Fmt.string ppf "\n"
  | Some Syntax.Markdown | None -> Fmt.string ppf "```\n"

let pp_legacy_labels ppf = function
  | [] -> ()
  | l -> Fmt.pf ppf " %a" Fmt.(list ~sep:(any ",") Label.pp) l

let pp_labels ?syntax ppf labels =
  match syntax with
  | Some Syntax.Mli | Some Syntax.Mld ->
      Fmt.(list ~sep:(any ",") Label.pp) ppf labels
  | Some Syntax.Cram -> (
      match labels with
      | [] -> ()
      | [ Non_det None ] -> Fmt.pf ppf "<-- non-deterministic\n"
      | [ Non_det (Some Nd_output) ] ->
          Fmt.pf ppf "<-- non-deterministic output\n"
      | [ Non_det (Some Nd_command) ] ->
          Fmt.pf ppf "<-- non-deterministic command\n"
      | _ -> failwith "cannot happen: checked during parsing")
  | Some Syntax.Markdown | None -> (
      match labels with
      | [] -> ()
      | l ->
          Fmt.pf ppf "<!-- $MDX %a -->\n" Fmt.(list ~sep:(any ",") Label.pp) l)

let pp_header ?syntax ppf t =
  match syntax with
  | Some Syntax.Mli | Some Syntax.Mld ->
      let lang_headers, other_labels =
        List.partition
          (function Label.Language_tag _ -> true | _ -> false)
          t.labels
      in
      let pp_lang_header ppf = function
        | [] -> ()
        | [ l ] -> Fmt.pf ppf "@%a" Label.pp l
        | _ -> failwith "Multiple language tags, unsupported"
      in
      let pp_labels ppf = function
        | [] -> ()
        | labels -> Fmt.pf ppf " %a" (pp_labels ?syntax) labels
      in
      Fmt.pf ppf "{%a%a%a["
        Fmt.(option string)
        t.delim pp_lang_header lang_headers pp_labels other_labels
  | Some Syntax.Cram -> pp_labels ?syntax ppf t.labels
  | Some Syntax.Markdown | None ->
      if t.legacy_labels then
        Fmt.pf ppf "```%a%a"
          Fmt.(option Header.pp)
          (header t) pp_legacy_labels t.labels
      else
        Fmt.pf ppf "%a```%a" (pp_labels ?syntax) t.labels
          Fmt.(option Header.pp)
          (header t)

let pp ?syntax ppf b =
  pp_header ?syntax ppf b;
  pp_contents ?syntax ppf b;
  pp_footer ?syntax ppf b

let directory t = t.dir
let file t = match t.value with Include t -> Some t.file_included | _ -> None

let non_det t =
  match t.value with
  | OCaml b -> b.non_det
  | Cram b -> b.non_det
  | Toplevel b -> b.non_det
  | Include _ | Raw _ -> None

let skip t = t.skip
let set_variables t = t.set_variables
let unset_variables t = t.unset_variables
let value t = t.value
let section t = t.section

let guess_ocaml_kind contents =
  let rec aux = function
    | [] -> `Code
    | h :: t ->
        let h = String.trim h in
        if h = "" then aux t
        else if String.length h > 1 && h.[0] = '#' then `Toplevel
        else `Code
  in
  aux contents

let rec ends_by_semi_semi = function
  | [] -> false
  | [ h ] -> Astring.String.is_suffix ~affix:";;" h
  | _ :: xs -> ends_by_semi_semi xs

let version_enabled version =
  let+ curr_version = Ocaml_version.of_string Sys.ocaml_version in
  match version with
  | Some (op, v) ->
      Label.Relation.compare op (Ocaml_version.compare curr_version v) 0
  | None -> true

let os_type_enabled os_type =
  match os_type with
  | Some (op, v) ->
      Label.Relation.compare op
        (String.compare
           (String.lowercase_ascii Sys.os_type)
           (String.lowercase_ascii v))
        0
  | None -> true

let get_label f (labels : Label.t list) = Util.List.find_map f labels

let label_not_allowed ~loc ~label ~kind =
  loc_error ~loc "`%s` label is not allowed for %s blocks." label kind

let label_required ~loc ~label ~kind =
  loc_error ~loc "`%s` label is required for %s blocks." label kind

let check_not_set ~loc msg = function
  | Some _ -> loc_error ~loc "%s" msg
  | None -> Ok ()

let check_no_errors ~loc = function
  | [] -> Ok ()
  | _ :: _ ->
      loc_error ~loc "error block cannot be attached to a non-OCaml block"

type block_config = {
  non_det : Label.non_det option;
  part : string option;
  env : string option;
  dir : string option;
  skip : bool;
  version : (Label.Relation.t * Ocaml_version.t) option;
  os_type : (Label.Relation.t * string) option;
  set_variables : (string * string) list;
  unset_variables : string list;
  file_inc : string option;
}

let get_block_config l =
  {
    non_det =
      get_label
        (function
          | Non_det (Some x) -> Some x
          | Non_det None -> Some Label.default_non_det
          | _ -> None)
        l;
    part = get_label (function Part x -> Some x | _ -> None) l;
    env = get_label (function Env x -> Some x | _ -> None) l;
    dir = get_label (function Dir x -> Some x | _ -> None) l;
    skip = List.exists (function Label.Skip -> true | _ -> false) l;
    version = get_label (function Version (x, y) -> Some (x, y) | _ -> None) l;
    os_type = get_label (function Os_type (x, y) -> Some (x, y) | _ -> None) l;
    set_variables =
      List.filter_map (function Label.Set (v, x) -> Some (v, x) | _ -> None) l;
    unset_variables =
      List.filter_map (function Label.Unset x -> Some x | _ -> None) l;
    file_inc = get_label (function File x -> Some x | _ -> None) l;
  }

let mk_ocaml ~loc ~config ~header ~contents ~errors =
  let kind = "OCaml" in
  match config with
  | { file_inc = None; part = None; env; non_det; _ } -> (
      (* TODO: why does this call guess_ocaml_kind when infer_block already did? *)
      match guess_ocaml_kind contents with
      | `Code -> Ok (OCaml { env = Ocaml_env.mk env; non_det; errors; header })
      | `Toplevel ->
          loc_error ~loc "toplevel syntax is not allowed in OCaml blocks.")
  | { file_inc = Some _; _ } -> label_not_allowed ~loc ~label:"file" ~kind
  | { part = Some _; _ } -> label_not_allowed ~loc ~label:"part" ~kind

let mk_cram ~loc ?language ~config ~header ~errors () =
  let kind = "shell" in
  match config with
  | { file_inc = None; part = None; env = None; non_det; _ } ->
      let+ () = check_no_errors ~loc errors in
      let language =
        Util.Option.value language
          ~default:
            (match header with
            | Some (Header.Shell language) -> language
            | _ -> `Sh)
      in
      Cram { language; non_det }
  | { file_inc = Some _; _ } -> label_not_allowed ~loc ~label:"file" ~kind
  | { part = Some _; _ } -> label_not_allowed ~loc ~label:"part" ~kind
  | { env = Some _; _ } -> label_not_allowed ~loc ~label:"env" ~kind

let mk_toplevel ~loc ~config ~contents ~errors =
  let kind = "toplevel" in
  match config with
  | { file_inc = None; part = None; env; non_det; _ } -> (
      match guess_ocaml_kind contents with
      | `Code -> loc_error ~loc "invalid toplevel syntax in toplevel blocks."
      | `Toplevel ->
          let+ () = check_no_errors ~loc errors in
          Toplevel { env = Ocaml_env.mk env; non_det })
  | { file_inc = Some _; _ } -> label_not_allowed ~loc ~label:"file" ~kind
  | { part = Some _; _ } -> label_not_allowed ~loc ~label:"part" ~kind

let mk_include ~loc ~config ~header ~errors =
  let kind = "include" in
  match config with
  | { file_inc = Some file_included; part; non_det = None; env = None; _ } -> (
      let* () = check_no_errors ~loc errors in
      match header with
      | Some Header.OCaml ->
          let file_kind = Fk_ocaml { part_included = part } in
          Ok (Include { file_included; file_kind })
      | _ -> (
          match part with
          | None ->
              let file_kind = Fk_other { header } in
              Ok (Include { file_included; file_kind })
          | Some _ ->
              label_not_allowed ~loc ~label:"part" ~kind:"non-OCaml include"))
  | { file_inc = None; _ } -> label_required ~loc ~label:"file" ~kind
  | { non_det = Some _; _ } ->
      label_not_allowed ~loc ~label:"non-deterministic" ~kind
  | { env = Some _; _ } -> label_not_allowed ~loc ~label:"env" ~kind

let infer_block ~loc ~config ~header ~contents ~errors =
  match config with
  | { file_inc = Some _; _ } -> mk_include ~loc ~config ~header ~errors
  | { file_inc = None; part; _ } -> (
      match header with
      | Some (Header.Shell language) ->
          mk_cram ~loc ~language ~config ~header ~errors ()
      | Some Header.OCaml -> (
          match guess_ocaml_kind contents with
          | `Code -> mk_ocaml ~loc ~config ~header ~contents ~errors
          | `Toplevel -> mk_toplevel ~loc ~config ~contents ~errors)
      | _ ->
          let* () =
            check_not_set ~loc "`part` label requires a `file` label." part
          in
          let+ () = check_no_errors ~loc errors in
          Raw { header })

let mk ~loc ~section ~labels ~legacy_labels ~header ~delim ~contents ~errors =
  let block_kind =
    get_label (function Block_kind x -> Some x | _ -> None) labels
  in
  let config = get_block_config labels in
  let* value =
    match block_kind with
    | Some OCaml -> mk_ocaml ~loc ~config ~header ~contents ~errors
    | Some Cram -> mk_cram ~loc ~config ~header ~errors ()
    | Some Toplevel -> mk_toplevel ~loc ~config ~contents ~errors
    | Some Include -> mk_include ~loc ~config ~header ~errors
    | None -> infer_block ~loc ~config ~header ~contents ~errors
  in
  let+ version_enabled = version_enabled config.version in
  let os_type_enabled = os_type_enabled config.os_type in
  {
    loc;
    section;
    dir = config.dir;
    labels;
    legacy_labels;
    contents;
    skip = config.skip;
    version_enabled;
    os_type_enabled;
    set_variables = config.set_variables;
    unset_variables = config.unset_variables;
    delim;
    value;
  }

let mk_include ~loc ~section ~labels =
  match get_label (function File x -> Some x | _ -> None) labels with
  | Some file_inc ->
      let header = Header.infer_from_file file_inc in
      mk ~loc ~section ~labels ~legacy_labels:false ~header ~contents:[]
        ~errors:[] ~delim:None
  | None -> label_required ~loc ~label:"file" ~kind:"include"

let parse_labels ~label_cmt ~legacy_labels =
  match (label_cmt, legacy_labels) with
  | Some label_cmt, "" ->
      let+ labels = Label.of_string label_cmt in
      (labels, false)
  | Some _, _ -> Error [ `Msg "cannot mix both block labels syntax" ]
  | None, l ->
      let+ labels = Label.of_string l in
      (labels, true)

let from_raw raw =
  match raw with
  | Raw.Include { loc; section; labels } ->
      let* labels = locate_errors ~loc (Label.of_string labels) in
      Util.Result.to_error_list @@ mk_include ~loc ~section ~labels
  | Raw.Any { loc; section; header; contents; label_cmt; legacy_labels; errors }
    ->
      let header = Header.of_string header in
      let* labels, legacy_labels =
        locate_errors ~loc (parse_labels ~label_cmt ~legacy_labels)
      in
      Util.Result.to_error_list
      @@ mk ~loc ~section ~header ~contents ~labels ~legacy_labels ~errors
           ~delim:None

let is_active ?section:s t =
  let active =
    match s with
    | Some p -> (
        match t.section with
        | Some s -> Re.execp (Re.Perl.compile_pat p) (snd s)
        | None -> Re.execp (Re.Perl.compile_pat p) "")
    | None -> true
  in
  active && t.version_enabled && t.os_type_enabled && not t.skip
