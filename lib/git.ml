open Import

module Ls_remote = struct
  let non_packed_suffix = "^{}"
  let ref_arg ref = Bos.Cmd.(v ref % (ref ^ non_packed_suffix))

  let parse_output_line s =
    match String.extract_blank_separated_words s with
    | [ commit; ref ] -> Ok (commit, ref)
    | _ ->
        Error (`Msg (Printf.sprintf "Invalid git ls-remote output line: %S" s))

  type search_result = {
    maybe_packed : string option;
    not_packed : string option;
  }

  let interpret_search_result sr =
    match sr with
    | { maybe_packed = None; not_packed = None } -> None
    | { maybe_packed = Some commit; not_packed = None }
    | { maybe_packed = _; not_packed = Some commit } ->
        Some commit

  let search_ref target lines =
    let target_not_packed = target ^ non_packed_suffix in
    let f acc (commit, full_ref) =
      if String.equal full_ref target then
        { acc with maybe_packed = Some commit }
      else if String.equal full_ref target_not_packed then
        { acc with not_packed = Some commit }
      else acc
    in
    List.fold_left ~f ~init:{ maybe_packed = None; not_packed = None } lines

  let is_commit target = List.exists ~f:(fun (commit, _) -> commit = target)

  let looks_like_commit ref =
    String.length ref > 6
    && Astring.(String.for_all Char.Ascii.is_hex_digit ref)

  let commit_pointed_by ~ref output_lines =
    let log_approx () =
      Logs.debug (fun l ->
          l "Ref '%s' looks like a commit but hasn't been found in the remote."
            ref);
      Ok ref
    in
    let open Result.O in
    match output_lines with
    | [ "" ] when looks_like_commit ref -> log_approx ()
    | [ "" ] -> Error `No_such_ref
    | _ -> (
        let* parsed_lines = Result.List.map ~f:parse_output_line output_lines in
        let search prefix =
          let result = search_ref (prefix ^ ref) parsed_lines in
          interpret_search_result result
        in
        match (search "refs/tags/", search "refs/heads/") with
        | Some _, Some _ -> Error `Multiple_such_refs
        | Some commit, None | None, Some commit -> Ok commit
        | None, None when is_commit ref parsed_lines -> Ok ref
        | None, None when looks_like_commit ref -> log_approx ()
        | None, None -> Error `No_such_ref)

  let parse_ref_output_line ~symref line =
    match String.extract_blank_separated_words line with
    | [ "ref:"; branch; ref ] when ref = symref -> Some branch
    | _ -> None

  let extract_branch branch_ref =
    String.drop_prefix branch_ref ~prefix:"refs/heads/"
    |> Option.to_result
         ~none:
           (`Msg
             (Printf.sprintf
                "Invalid `git ls-remote --symref` output. Failed to extract \
                 branch from ref `%s`."
                branch_ref))

  let branch_of_symref ~symref output_lines =
    match List.filter_map ~f:(parse_ref_output_line ~symref) output_lines with
    | [] -> Error `Not_a_symref
    | [ ref ] -> extract_branch ref
    | _ ->
        Error
          (`Msg
            "Invalid `git ls-remote --symref` output. Too many lines starting \
             by `ref:`.")
end

module Ref = struct
  type t = string

  let equal = String.equal
  let compare = String.compare
  let pp = Format.pp_print_string

  type resolved = { t : t; commit : string }

  let equal_resolved r r' = equal r.t r'.t && String.equal r.commit r'.commit

  let pp_resolved fmt resolved =
    Format.fprintf fmt "@[<hov 2>{ t = %a;@ commit = %s }@]" pp resolved.t
      resolved.commit
end
