open Import

module type FIELD_SHAPE = sig
  type t

  val name : string
  val shape : t Serial_shape.t
  val merge : t list -> (t, Rresult.R.msg) result
end

module type FIELD = sig
  type t

  val set : t -> OpamFile.OPAM.t -> OpamFile.OPAM.t
  val get : OpamFile.OPAM.t -> (t option, Rresult.R.msg) result
  val merge : t list -> (t, Rresult.R.msg) result
  val add_arg : t option Cmdliner.Arg.t
  val overwrite_arg : t option Cmdliner.Arg.t

  (**/**)

  (** Exposed for testing purposes only *)

  val from_opam_value :
    OpamParserTypes.FullPos.value -> (t, Rresult.R.msg) result

  val to_opam_value : t -> OpamParserTypes.FullPos.value
  val cmdliner_conv : t Cmdliner.Arg.conv

  (**/**)
end

module Make_field (X : FIELD_SHAPE) : FIELD with type t = X.t = struct
  type t = X.t

  let to_opam_value t = Serial_shape.to_opam_val X.shape t
  let from_opam_value value = Serial_shape.from_opam_val X.shape value

  let field =
    Opam.Extra_field.make ~name:X.name
      ~to_opam_value:(Serial_shape.to_opam_val X.shape)
      ~from_opam_value:(Serial_shape.from_opam_val X.shape)

  let set t opam = Opam.Extra_field.set field t opam
  let get opam = Opam.Extra_field.get field opam
  let merge = X.merge
  let cmdliner_conv = Serial_shape.cmdliner_conv X.shape

  let add_arg =
    let docv = String.uppercase_ascii X.name in
    let long_name = Printf.sprintf "--add-%s" X.name in
    let doc =
      Printf.sprintf
        "CLI equivalent of the %s extension. Use this to complement the \
         corresponding extensions in your local opam files."
        (Opam.Extra_field.name field)
    in
    Cmdliner.Arg.(opt (some cmdliner_conv) None (info ~doc ~docv [ long_name ]))

  let overwrite_arg =
    let docv = String.uppercase_ascii X.name in
    let long_name = Printf.sprintf "--%s" X.name in
    let doc =
      Printf.sprintf
        "CLI equivalent of the %s extension. Use this to replace the \
         corresponding extensions in your local opam files."
        (Opam.Extra_field.name field)
    in
    Cmdliner.Arg.(opt (some cmdliner_conv) None (info ~doc ~docv [ long_name ]))
end

module Opam_repositories_shape = struct
  type t = OpamUrl.Set.t

  let name = "opam-repositories"

  let from_repr l =
    let urls = List.map ~f:OpamUrl.of_string l in
    Ok (OpamUrl.Set.of_list urls)

  let to_repr url_set =
    OpamUrl.Set.elements url_set |> List.map ~f:OpamUrl.to_string

  let conv = Serial_shape.Conv.make ~from_repr ~to_repr ()
  let shape = Serial_shape.conv conv Serial_shape.(list string)

  let merge = function
    | [] -> Ok OpamUrl.Set.empty
    | hd :: tl -> Ok (List.fold_left tl ~init:hd ~f:OpamUrl.Set.union)
end

module Opam_repositories = Make_field (Opam_repositories_shape)

module Opam_global_vars_shape = struct
  type t = OpamVariable.variable_contents String.Map.t

  let name = "global-opam-vars"

  let var_content_from_repr choice =
    match choice with
    | `C1 b -> Ok (OpamVariable.B b)
    | `C2 l -> Ok (OpamVariable.L l)
    | `C3 s -> Ok (OpamVariable.S s)

  let var_content_to_repr var =
    match (var : OpamVariable.variable_contents) with
    | B b -> `C1 b
    | L l -> `C2 l
    | S s -> `C3 s

  let var_content_conv =
    Serial_shape.Conv.make ~from_repr:var_content_from_repr
      ~to_repr:var_content_to_repr ()

  exception Env_var_defined_twice of string

  let from_repr l =
    try
      List.fold_left l ~init:String.Map.empty ~f:(fun acc (name, content) ->
          String.Map.update acc name ~f:(function
            | None -> Some content
            | Some _ -> raise (Env_var_defined_twice name)))
      |> Result.ok
    with Env_var_defined_twice name ->
      Rresult.R.error_msgf "Opam global variable %s is defined more than once"
        name

  let to_repr map = String.Map.bindings map
  let conv = Serial_shape.Conv.make ~from_repr ~to_repr ()

  let shape =
    let var_shape =
      Serial_shape.conv var_content_conv
        Serial_shape.(choice3 bool (list string) string)
    in
    Serial_shape.conv conv Serial_shape.(list (pair string var_shape))

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
        with Env_var_defined_twice var_name ->
          Rresult.R.error_msgf
            "Environment variable %s is set to different values in different \
             opam files %s field"
            var_name name)
end

module Opam_global_vars = Make_field (Opam_global_vars_shape)

module Opam_provided_shape = struct
  type t = OpamPackage.Name.Set.t

  let name = "opam-provided"

  let from_repr repr =
    let l = match repr with `C1 l -> l | `C2 s -> [ s ] in
    l
    |> List.map ~f:OpamPackage.Name.of_string
    |> OpamPackage.Name.Set.of_list |> Result.ok

  let to_repr t =
    t |> OpamPackage.Name.Set.elements |> List.map ~f:OpamPackage.Name.to_string
    |> fun l -> `C1 l

  let conv = Serial_shape.Conv.make ~from_repr ~to_repr ()
  let shape = Serial_shape.conv conv Serial_shape.(choice2 (list string) string)

  let merge = function
    | [] -> Ok OpamPackage.Name.Set.empty
    | init :: xs -> Ok (List.fold_left xs ~init ~f:OpamPackage.Name.Set.union)
end

module Opam_provided = Make_field (Opam_provided_shape)

module Opam_repositories_url_rewriter = struct
  let opam_monorepo_cwd_var = "$OPAM_MONOREPO_CWD"
  let opam_monorepo_cwd_var_len = String.length opam_monorepo_cwd_var

  let rewrite_one_in ~opam_monorepo_cwd url =
    let error () =
      Rresult.R.error_msgf
        "$OPAM_MONOREPO_CWD can only be used to rewrite the root part of \
         file:// URLs. %S is an invalid use of the variable."
        (OpamUrl.to_string url)
    in
    let idx =
      Astring.String.find_sub ~start:0 ~sub:opam_monorepo_cwd_var
        url.OpamUrl.path
    in
    match ((url : OpamUrl.t), idx) with
    | { transport = "file"; path; _ }, Some 0 ->
        let path_remainder =
          String.sub ~pos:opam_monorepo_cwd_var_len
            ~len:(String.length path - opam_monorepo_cwd_var_len)
            path
        in
        let rewritten_path = opam_monorepo_cwd ^ path_remainder in
        Ok { url with OpamUrl.path = rewritten_path }
    | { transport = _; _ }, Some _ -> error ()
    | _, None -> Ok url

  let rewrite_one_out ~opam_monorepo_cwd url =
    match url.OpamUrl.transport with
    | "file" -> (
        let path_remainder =
          String.drop_prefix ~prefix:opam_monorepo_cwd url.OpamUrl.path
        in
        match path_remainder with
        | Some r ->
            let rewritten_path = opam_monorepo_cwd_var ^ r in
            { url with OpamUrl.path = rewritten_path }
        | None -> url)
    | _ -> url

  let rewrite_in ~opam_monorepo_cwd repositories_opt =
    let open Result.O in
    match repositories_opt with
    | None -> Ok None
    | Some rs ->
        let l = OpamUrl.Set.elements rs in
        let* rewritten =
          Result.List.map ~f:(rewrite_one_in ~opam_monorepo_cwd) l
        in
        let set = OpamUrl.Set.of_list rewritten in
        Ok (Some set)

  let rewrite_out ~opam_monorepo_cwd repositories_opt =
    match repositories_opt with
    | None -> None
    | Some rs ->
        let seq = OpamUrl.Set.to_seq rs in
        let rewritten = Seq.map (rewrite_one_out ~opam_monorepo_cwd) seq in
        Some (OpamUrl.Set.of_seq rewritten)
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
  let* repositories = Opam_repositories.get opam_file in
  let* opam_provided = Opam_provided.get opam_file in
  let* repositories =
    Opam_repositories_url_rewriter.rewrite_in ~opam_monorepo_cwd repositories
  in
  Ok { global_vars; repositories; opam_provided }

let set_field set var opam_file =
  Option.map_default ~default:opam_file var ~f:(fun v -> set v opam_file)

let set ~opam_monorepo_cwd { global_vars; repositories; opam_provided }
    opam_file =
  let opam_monorepo_cwd = opam_monorepo_cwd_from_root opam_monorepo_cwd in
  let repositories =
    Opam_repositories_url_rewriter.rewrite_out ~opam_monorepo_cwd repositories
  in
  opam_file
  |> set_field Opam_global_vars.set global_vars
  |> set_field Opam_repositories.set repositories
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

let cli_add_config =
  let open Cmdliner.Term in
  const (fun global_vars repositories opam_provided ->
      { global_vars; repositories; opam_provided })
  $ Cmdliner.Arg.value Opam_global_vars.add_arg
  $ Cmdliner.Arg.value Opam_repositories.add_arg
  $ Cmdliner.Arg.value Opam_provided.add_arg

let cli_overwrite_config =
  let open Cmdliner.Term in
  const (fun global_vars repositories opam_provided ->
      { global_vars; repositories; opam_provided })
  $ Cmdliner.Arg.value Opam_global_vars.overwrite_arg
  $ Cmdliner.Arg.value Opam_repositories.overwrite_arg
  $ Cmdliner.Arg.value Opam_provided.overwrite_arg

let make ~overwrite_config ~add_config ~local_opam_files_config =
  let open Result.O in
  let* global_vars =
    match overwrite_config.global_vars with
    | Some _ as g -> Ok g
    | None ->
        merge_field Opam_global_vars.merge add_config.global_vars
          local_opam_files_config.global_vars
  in
  let* repositories =
    match overwrite_config.repositories with
    | Some _ as r -> Ok r
    | None ->
        merge_field Opam_repositories.merge add_config.repositories
          local_opam_files_config.repositories
  in
  let* opam_provided =
    match overwrite_config.opam_provided with
    | Some _ as o -> Ok o
    | None ->
        merge_field Opam_provided.merge add_config.opam_provided
          local_opam_files_config.opam_provided
  in
  Ok { global_vars; repositories; opam_provided }

module Private = struct
  (** Re-expose private functions for testing purposes *)

  module Opam_repositories = struct
    let from_opam_value = Opam_repositories.from_opam_value
    let to_opam_value = Opam_repositories.to_opam_value
    let cmdliner_conv = Opam_repositories.cmdliner_conv
  end

  module Opam_global_vars = struct
    let from_opam_value = Opam_global_vars.from_opam_value
    let to_opam_value = Opam_global_vars.to_opam_value
    let cmdliner_conv = Opam_global_vars.cmdliner_conv
  end

  module Opam_provided = struct
    let from_opam_value = Opam_provided.from_opam_value
    let to_opam_value = Opam_provided.to_opam_value
    let cmdliner_conv = Opam_provided.cmdliner_conv
  end

  module Opam_repositories_url_rewriter = struct
    let rewrite_one_in = Opam_repositories_url_rewriter.rewrite_one_in
    let rewrite_one_out = Opam_repositories_url_rewriter.rewrite_one_out
  end
end
