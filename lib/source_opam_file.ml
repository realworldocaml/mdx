open Import

module type EXTENSION = sig
  (** The type of modules used to extract data stored in opam extensions *)

  type t
  (** The type of the data encoded in the extension *)

  val field : t Opam.Extra_field.t

  val merge : t list -> (t, Rresult.R.msg) result
  (** Function to merge the extensions into a single one when it appears
    in the metadata of several target packages *)
end

module Opam_repositories = struct
  type t = OpamUrl.Set.t

  let url_from_opam_value value =
    let open Result.O in
    let+ url_string = Opam.Value.String.from_value value in
    OpamUrl.of_string url_string

  let url_to_opam_value url = Opam.Value.String.to_value (OpamUrl.to_string url)

  let from_opam_value value =
    let open Result.O in
    let+ l = Opam.Value.List.from_value url_from_opam_value value in
    OpamUrl.Set.of_list l

  let to_opam_value t =
    let elements = OpamUrl.Set.elements t in
    Opam.Value.List.to_value url_to_opam_value elements

  let field =
    Opam.Extra_field.make ~name:"opam-repositories" ~from_opam_value
      ~to_opam_value

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

type config = {
  global_vars : Opam_global_vars.t option;
  repositories : Opam_repositories.t option;
}

let extract_config opam_file =
  let open Result.O in
  let* global_vars = Opam.Extra_field.get Opam_global_vars.field opam_file in
  let* repositories = Opam.Extra_field.get Opam_repositories.field opam_file in
  Ok { global_vars; repositories }

let set_field field var opam_file =
  Option.map_default ~default:opam_file var ~f:(fun v ->
      Opam.Extra_field.set field v opam_file)

let set_config config opam_file =
  opam_file
  |> set_field Opam_global_vars.field config.global_vars
  |> set_field Opam_repositories.field config.repositories

let merge_field f a b =
  let open Result.O in
  match (a, b) with
  | None, None -> Ok None
  | Some x, None | None, Some x -> Ok (Some x)
  | Some x, Some x' ->
      let+ merged = f [ x; x' ] in
      Some merged

let merge_config_pair config config' =
  let open Result.O in
  let* global_vars =
    merge_field Opam_global_vars.merge config.global_vars config'.global_vars
  in
  let* repositories =
    merge_field Opam_repositories.merge config.repositories config'.repositories
  in
  Ok { global_vars; repositories }

let merge_config = function
  | [] -> Ok { global_vars = None; repositories = None }
  | hd :: tl ->
      Result.List.fold_left tl ~init:hd ~f:(fun config config' ->
          merge_config_pair config config')
