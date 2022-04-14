open Import

module Opam_repositories = struct
  type t = OpamUrl.Set.t

  let opam_monorepo_cwd_var = "$OPAM_MONOREPO_CWD"

  let opam_monorepo_cwd_var_regexp =
    let open Re in
    compile (seq [ str "file://"; str opam_monorepo_cwd_var ])

  let rewrite_url_in ~pos ~opam_monorepo_cwd url_str =
    let rewritten =
      Re.replace_string opam_monorepo_cwd_var_regexp
        ~by:("file://" ^ opam_monorepo_cwd)
        url_str
    in
    if Astring.String.is_infix ~affix:opam_monorepo_cwd_var rewritten then
      Opam.Pos.errorf ~pos
        "$OPAM_MONOREPO_CWD can only be used to rewrite the root part of \
         file:// URLs"
    else Ok rewritten

  let rewrite_url_out ~opam_monorepo_cwd_regexp url_str =
    Re.replace_string opam_monorepo_cwd_regexp
      ~by:("file://" ^ opam_monorepo_cwd_var)
      url_str

  let url_from_opam_value ~opam_monorepo_cwd value =
    let open Result.O in
    let* url_string = Opam.Value.String.from_value value in
    let pos = value.OpamParserTypes.FullPos.pos in
    let* rewritten = rewrite_url_in ~pos ~opam_monorepo_cwd url_string in
    Ok (OpamUrl.of_string rewritten)

  let url_to_opam_value ~opam_monorepo_cwd_regexp url =
    Opam.Value.String.to_value
      (rewrite_url_out ~opam_monorepo_cwd_regexp (OpamUrl.to_string url))

  let from_opam_value ~opam_monorepo_cwd value =
    let open Result.O in
    let+ l =
      Opam.Value.List.from_value (url_from_opam_value ~opam_monorepo_cwd) value
    in
    OpamUrl.Set.of_list l

  let to_opam_value ~opam_monorepo_cwd =
    let opam_monorepo_cwd_regexp =
      Re.(compile (seq [ str "file://"; str opam_monorepo_cwd ]))
    in
    fun t ->
      let elements = OpamUrl.Set.elements t in
      Opam.Value.List.to_value
        (url_to_opam_value ~opam_monorepo_cwd_regexp)
        elements

  let field ~opam_monorepo_cwd =
    Opam.Extra_field.make ~name:"opam-repositories"
      ~from_opam_value:(from_opam_value ~opam_monorepo_cwd)
      ~to_opam_value:(to_opam_value ~opam_monorepo_cwd)

  let get ~opam_monorepo_cwd opam =
    let field = field ~opam_monorepo_cwd in
    Opam.Extra_field.get field opam

  let set ~opam_monorepo_cwd t opam =
    let field = field ~opam_monorepo_cwd in
    Opam.Extra_field.set field t opam

  let merge = function
    | [] -> Ok OpamUrl.Set.empty
    | hd :: tl -> Ok (List.fold_left tl ~init:hd ~f:OpamUrl.Set.union)
end

module Opam_global_vars = struct
  type t = OpamVariable.variable_contents String.Map.t

  exception Env_var_defined_twice of string

  let variable_content_from_opam_value value =
    let open Result.O in
    match (value : OpamParserTypes.FullPos.value) with
    | { pelem = Bool b; _ } -> Ok (OpamVariable.B b)
    | { pelem = String s; _ } -> Ok (OpamVariable.S s)
    | { pelem = List _; _ } ->
        let+ l =
          Opam.Value.List.from_value Opam.Value.String.from_value value
        in
        OpamVariable.L l
    | _ ->
        Opam.Pos.unexpected_value_error
          ~expected:"a boolean, a string or list of strings" value

  let variable_content_to_opam_value variable_content =
    match (variable_content : OpamVariable.variable_contents) with
    | B b -> Opam.Pos.with_default (OpamParserTypes.FullPos.Bool b)
    | S s -> Opam.Value.String.to_value s
    | L l -> Opam.Value.List.to_value Opam.Value.String.to_value l

  let from_opam_value_one value =
    let open Result.O in
    let* l = Opam.Value.List.from_value Result.ok value in
    match l with
    | [ name; content ] ->
        let* name = Opam.Value.String.from_value name in
        let+ content = variable_content_from_opam_value content in
        (name, content)
    | _ ->
        Opam.Pos.unexpected_value_error
          ~expected:"a list with a var name followed by the var content" value

  let to_opam_value_one (name, content) =
    let name = Opam.Value.String.to_value name in
    let content = variable_content_to_opam_value content in
    Opam.Value.List.to_value Fun.id [ name; content ]

  let from_bindings ~pos l =
    try
      List.fold_left l ~init:String.Map.empty ~f:(fun acc (name, content) ->
          String.Map.update acc name ~f:(function
            | None -> Some content
            | Some _ -> raise (Env_var_defined_twice name)))
      |> Result.ok
    with Env_var_defined_twice name ->
      Opam.Pos.errorf ~pos "Env variable %s is defined more than once" name

  let from_opam_value value =
    let open Result.O in
    let pos = value.OpamParserTypes.FullPos.pos in
    let* bindings = Opam.Value.List.from_value from_opam_value_one value in
    from_bindings ~pos bindings

  let to_opam_value t =
    let bindings = String.Map.bindings t in
    Opam.Value.List.to_value to_opam_value_one bindings

  let field =
    Opam.Extra_field.make ~name:"global-opam-vars" ~to_opam_value
      ~from_opam_value

  let get opam = Opam.Extra_field.get field opam
  let set t opam = Opam.Extra_field.set field t opam

  let merge = function
    | [] -> Ok String.Map.empty
    | hd :: tl -> (
        try
          List.fold_left tl ~init:hd ~f:(fun acc vars ->
              String.Map.merge acc vars ~f:(fun name content content' ->
                  match (content, content') with
                  | None, None -> assert false
                  | Some c, None | None, Some c -> Some c
                  | Some c, Some c' when c = c' -> Some c
                  | Some _, Some _ -> raise (Env_var_defined_twice name)))
          |> Result.ok
        with Env_var_defined_twice name ->
          Rresult.R.error_msgf
            "Environment variable %s is set to different values in different \
             opam files %s field"
            name
            (Opam.Extra_field.name field))
end

module Opam_provided = struct
  let from_opam_value opam_chunk =
    let open Result.O in
    let* names =
      match (opam_chunk : OpamParserTypes.FullPos.value) with
      | { pelem = String s; _ } -> Ok [ s ]
      | { pelem = List items; _ } ->
          items.pelem
          |> List.map ~f:(fun item ->
                 match (item : OpamParserTypes.FullPos.value) with
                 | { pelem = String s; _ } -> Ok s
                 | otherwise ->
                     Opam.Pos.unexpected_value_error ~expected:"a string"
                       otherwise)
          |> Result.List.all
      | otherwise ->
          Opam.Pos.unexpected_value_error
            ~expected:"a string or a list of strings" otherwise
    in
    let names = List.map ~f:OpamPackage.Name.of_string names in
    Ok (OpamPackage.Name.Set.of_list names)

  let to_opam_value names =
    OpamPackage.Name.Set.elements names
    |> List.map ~f:OpamPackage.Name.to_string
    |> Opam.Value.List.to_value Opam.Value.String.to_value

  let field =
    Opam.Extra_field.make ~name:"opam-provided" ~to_opam_value ~from_opam_value

  let set t opam = Opam.Extra_field.set field t opam
  let get opam = Opam.Extra_field.get field opam

  let merge = function
    | [] -> Ok OpamPackage.Name.Set.empty
    | init :: xs -> Ok (List.fold_left xs ~init ~f:OpamPackage.Name.Set.union)
end

type t = {
  global_vars : Opam_global_vars.t option;
  repositories : Opam_repositories.t option;
  opam_provided : OpamPackage.Name.Set.t option;
}

let opam_monorepo_cwd_from_root path =
  if Fpath.is_rel path then
    invalid_arg "OPAM_MONOREPO_CWD must be an absolute path";
  Fpath.(to_string (normalize path))

let get ~opam_monorepo_cwd opam_file =
  let open Result.O in
  let opam_monorepo_cwd = opam_monorepo_cwd_from_root opam_monorepo_cwd in
  let* global_vars = Opam_global_vars.get opam_file in
  let* repositories = Opam_repositories.get ~opam_monorepo_cwd opam_file in
  let* opam_provided = Opam_provided.get opam_file in
  Ok { global_vars; repositories; opam_provided }

let set_field set var opam_file =
  Option.map_default ~default:opam_file var ~f:(fun v -> set v opam_file)

let set ~opam_monorepo_cwd { global_vars; repositories; opam_provided }
    opam_file =
  let opam_monorepo_cwd = opam_monorepo_cwd_from_root opam_monorepo_cwd in
  opam_file
  |> set_field Opam_global_vars.set global_vars
  |> set_field (Opam_repositories.set ~opam_monorepo_cwd) repositories
  |> set_field Opam_provided.set opam_provided

let merge_field f a b =
  let open Result.O in
  match (a, b) with
  | None, None -> Ok None
  | Some x, None | None, Some x -> Ok (Some x)
  | Some x, Some x' ->
      let+ merged = f [ x; x' ] in
      Some merged

let merge_pair t t' =
  let open Result.O in
  let* global_vars =
    merge_field Opam_global_vars.merge t.global_vars t'.global_vars
  in
  let* repositories =
    merge_field Opam_repositories.merge t.repositories t'.repositories
  in
  let* opam_provided =
    merge_field Opam_provided.merge t.opam_provided t'.opam_provided
  in
  Ok { global_vars; repositories; opam_provided }

let merge = function
  | [] -> Ok { global_vars = None; repositories = None; opam_provided = None }
  | hd :: tl ->
      Result.List.fold_left tl ~init:hd ~f:(fun t t' -> merge_pair t t')

module Private = struct
  (** Re-expose private functions for testing purposes *)

  module Opam_repositories = struct
    let from_opam_value = Opam_repositories.from_opam_value
    let to_opam_value = Opam_repositories.to_opam_value
  end

  module Opam_global_vars = struct
    let from_opam_value = Opam_global_vars.from_opam_value
    let to_opam_value = Opam_global_vars.to_opam_value
  end
end
