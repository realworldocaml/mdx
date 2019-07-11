open Stdune

module Dev_repo = struct
  type vcs = Git | Other of string

  let equal_vcs vcs vcs' =
    match (vcs, vcs') with
    | Git, Git -> true
    | Other s, Other s' -> String.equal s s'
    | (Git | Other _), _ -> false

  let pp_vcs fmt = function
    | Git -> Format.fprintf fmt "Git"
    | Other s -> Format.fprintf fmt "Other %S" s

  let vcs_from_string = function "git" -> Git | s -> Other s

  let known_vcs_from_string = function "git" -> Some Git | _ -> None

  type t = { vcs : vcs option; uri : Uri.t }

  let equal t t' =
    let { vcs; uri }, { vcs = vcs'; uri = uri' } = (t, t') in
    Option.equal equal_vcs vcs vcs' && Uri.equal uri uri'

  let pp fmt { vcs; uri } =
    let open Pp_combinators.Ocaml in
    Format.fprintf fmt "@[<hov 2>{ vcs = %a;@ uri = %a }@]" (option ~brackets:true pp_vcs) vcs
      Uri.pp uri

  let fallback_vcs_from_uri uri =
    let open Option.O in
    let vcs_from_scheme = Uri.scheme uri >>= known_vcs_from_string in
    match vcs_from_scheme with
    | Some vcs -> Some vcs
    | None -> if Uri_utils.has_git_extension uri then Some Git else None

  let from_string dev_repo =
    match Astring.String.cut ~sep:"+" dev_repo with
    | None ->
        let uri = Uri.of_string dev_repo in
        let vcs = fallback_vcs_from_uri uri in
        { vcs; uri }
    | Some (vcs, no_vcs_scheme_dev_repo) ->
        let uri = Uri.of_string no_vcs_scheme_dev_repo in
        let vcs = Some (vcs_from_string vcs) in
        { vcs; uri }
end
