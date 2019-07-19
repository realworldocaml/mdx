open Stdune
open Sexplib.Conv

module Ls_remote = struct
  let non_packed_suffix = "^{}"

  let ref_arg ref = Bos.Cmd.(v ref % (ref ^ non_packed_suffix))

  let parse_output_line s =
    match String.extract_blank_separated_words s with
    | [ commit; ref ] -> Ok (commit, ref)
    | _ -> Error (`Msg (Printf.sprintf "Invalid git ls-remote output line: %S" s))

  let commit_pointed_by ~ref output_lines =
    let open Result.O in
    let non_packed = ref ^ non_packed_suffix in
    let is_ref full_ref = String.is_suffix ~suffix:ref full_ref in
    let is_non_packed_ref full_ref = String.is_suffix ~suffix:non_packed full_ref in
    let rec go acc = function
      | [] -> acc
      | (commit, full_ref) :: _ when is_non_packed_ref full_ref -> Ok commit
      | (commit, full_ref) :: tl when is_ref full_ref -> go (Ok commit) tl
      | _ :: tl -> go acc tl
    in
    Result.List.map ~f:parse_output_line output_lines >>= fun parsed_lines ->
    go (Error `No_such_ref) parsed_lines
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
