open Astring

let depends_on_dune (formula : OpamTypes.filtered_formula) =
  let dune = OpamPackage.Name.of_string "dune" in
  let jbuilder = OpamPackage.Name.of_string "jbuilder" in
  let is_duneish name =
    let eq n n' = OpamPackage.Name.compare n n' = 0 in
    eq dune name || eq jbuilder name
  in
  OpamFormula.fold_left (fun acc (name, _) -> acc || is_duneish name) false formula

module Dev_repo = struct
  open Stdune

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

let strip_ext fname =
  let open Fpath in
  let fname = v fname |> rem_ext in
  let fname = if has_ext "tar" fname then rem_ext fname else fname in
  to_string fname

let tag_from_archive archive =
  let uri = Uri.of_string archive in
  let path = String.cuts ~empty:false ~sep:"/" (Uri.path uri) in
  let parse_err () =
    Logs.err (fun l -> l "Unable to classify archive %s" archive);
    None
  in
  let tag_of_file ?(prefix = "") f =
    match strip_ext f |> String.cut ~rev:true ~sep:"-" with
    | None -> parse_err ()
    | Some (_n, v) -> Some (prefix ^ v)
  in
  let tag_of_last_path ?prefix () =
    match List.rev path with file :: _ -> tag_of_file ?prefix file | [] -> None
  in
  match Uri.scheme uri with
  | Some "git+http" | Some "git+https" | Some "git+ssh" | Some "git" -> (
      match String.cuts ~empty:false ~sep:"#" archive with
      | [ _repo; tag ] -> Some tag
      | _ -> Some "master" )
  | Some "git+file" -> None
  | _ -> (
      match Uri.host uri with
      | Some "github.com" -> (
          match path with
          | [ _u; _r; "releases"; "download"; v; _archive ] -> Some v
          | [ _u; _r; "archive"; archive ] -> Some (strip_ext archive)
          | [ _u; _r; "archive"; tag; _ ] -> Some tag
          | _ -> if Uri.scheme uri = Some "git+https" then None else parse_err () )
      | Some "ocaml.janestreet.com" -> (
          match path with
          | [ "ocaml-core"; _ver; "files"; f ]
          | [ "ocaml-core"; _ver; "individual"; f ] -> tag_of_file f
          | [ "janestreet"; _r; "releases"; "download"; v; _f ] -> Some v
          | [ "janestreet"; _r; "archive"; f ] -> Some (strip_ext f)
          | _ -> parse_err () )
      | Some "gitlab.camlcity.org" | Some "download.camlcity.org" -> tag_of_last_path ()
      | Some "gitlab.inria.fr" -> (
          match path with [ _u; _r; "repository"; v; _archive ] -> Some v | _ -> parse_err () )
      | Some "ocamlgraph.lri.fr" | Some "erratique.ch" -> tag_of_last_path ~prefix:"v" ()
      | _ ->
          Logs.info (fun l ->
              l "Attempting to guess tag for %s from the final part of the URL" archive);
          tag_of_last_path () )

let classify_from_url_src src =
  let src = match String.cut ~sep:"#" src with None -> src | Some (src, _) -> src in
  match src with
  | "" -> None
  | src -> (
      match Dev_repo.from_string src with
      | { vcs = Some Git; uri = dev_repo_uri } -> (
          match Uri.host dev_repo_uri with
          | Some _host -> Some (`Git (Uri.to_string dev_repo_uri))
          | None -> Some (`Error "url.src without host") )
      | { vcs = None | Some (Other _); _ } -> None )

let classify_from_dev_repo ~name src =
  match src with
  | None ->
      Logs.debug (fun l -> l "Mapped %s to a virtual package as it has a blank dev repo" name);
      `Virtual
  | Some src -> (
      match Dev_repo.from_string src with
      | { vcs = Some Git; uri = dev_repo_uri } -> (
          match Uri.host dev_repo_uri with
          | Some _host -> `Git (Uri.to_string dev_repo_uri)
          | None -> `Error "dev-repo without host" )
      | { vcs = None | Some (Other _); _ } -> `Error "dev-repo doesn't use git as a VCS" )

let get_git_url ~src ~dev_repo ~name =
  Logs.debug (fun l -> l "Parsing git url from url.src (%s)" name);
  match classify_from_url_src src with
  | Some x -> x
  | None ->
      Logs.debug (fun l -> l "Falling back to parsing git url from dev-repo (%s)" name);
      classify_from_dev_repo ~name dev_repo

let classify_package ~package ~dev_repo ~archive () =
  if List.mem package.Types.Opam.name Config.base_packages then (`Virtual, None)
  else
    match archive with
    | None ->
        Logs.debug (fun l -> l "Mapped %s to a virtual package as it has no archive" package.name);
        (`Virtual, None)
    | Some archive -> (
        match get_git_url ~src:archive ~dev_repo ~name:package.name with
        | `Git _ as kind ->
            let tag = tag_from_archive archive in
            Logs.debug (fun l ->
                l "Mapped %s -> %s" archive (match tag with None -> "??" | Some v -> v));
            (kind, tag)
        | x -> (x, None) )
