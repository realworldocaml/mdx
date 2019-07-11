open Stdune
open Sexplib.Conv

let parse_ls_remote_line s =
  match String.extract_blank_separated_words s with
  | [ commit; ref ] -> Ok (commit, ref)
  | _ -> Error (`Msg (Printf.sprintf "Invalid git ls-remote output line: %S" s))

module Ref = struct
  type t = string [@@deriving sexp]

  let equal = String.equal

  let pp = Format.pp_print_string

  type resolved = { t : t; commit : string } [@@deriving sexp]

  let equal_resolved r r' = equal r.t r'.t && String.equal r.commit r'.commit

  let pp_resolved fmt resolved =
    Format.fprintf fmt "@[<hov 2>{ t = %a;@ commit = %s }@]" pp resolved.t resolved.commit
end
