open Stdune
open Sexplib.Conv

module Ls_remote = struct
  let non_packed_suffix = "^{}"

  let ref_arg ref = Bos.Cmd.(v ref % (ref ^ non_packed_suffix))

  let parse_output_line s =
    match String.extract_blank_separated_words s with
    | [ commit; ref ] -> Ok (commit, ref)
    | _ -> Error (`Msg (Printf.sprintf "Invalid git ls-remote output line: %S" s))

  type search_result = { maybe_packed : string option; not_packed : string option }

  let interpret_search_result sr =
    match sr with
    | { maybe_packed = None; not_packed = None } -> Error `No_such_ref
    | { maybe_packed = Some commit; not_packed = None }
    | { maybe_packed = _; not_packed = Some commit } ->
        Ok commit

  let commit_pointed_by ~ref output_lines =
    let open Result.O in
    let non_packed = ref ^ non_packed_suffix in
    let is_ref full_ref = String.is_suffix ~suffix:ref full_ref in
    let is_non_packed_ref full_ref = String.is_suffix ~suffix:non_packed full_ref in
    let rec go acc lines =
      match (acc, lines) with
      | _, [] -> Ok acc
      | { not_packed = None; _ }, (commit, full_ref) :: tl when is_non_packed_ref full_ref ->
          go { acc with not_packed = Some commit } tl
      | { maybe_packed = None; _ }, (commit, full_ref) :: tl when is_ref full_ref ->
          go { acc with maybe_packed = Some commit } tl
      | _, (_, full_ref) :: _ when is_non_packed_ref full_ref || is_ref full_ref ->
          Error `Points_to_several_commits
      | _, _ :: tl -> go acc tl
    in
    Result.List.map ~f:parse_output_line output_lines >>= fun parsed_lines ->
    go { maybe_packed = None; not_packed = None } parsed_lines >>= fun search_result ->
    interpret_search_result search_result
end

module Ref = struct
  type t = string [@@deriving sexp]

  let equal = String.equal

  let pp = Format.pp_print_string

  type resolved = { t : t; commit : string } [@@deriving sexp]

  let equal_resolved r r' = equal r.t r'.t && String.equal r.commit r'.commit

  let pp_resolved fmt resolved =
    Format.fprintf fmt "@[<hov 2>{ t = %a;@ commit = %s }@]" pp resolved.t resolved.commit
end
