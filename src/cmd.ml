(* Copyright (c) 2018 Anil Madhavapeddy <anil@recoil.org>
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
 *
 *)

open Bos
open Rresult
open Astring

let rec iter fn l =
  match l with hd :: tl -> fn hd >>= fun () -> iter fn tl | [] -> Ok ()

let map fn l =
  List.map fn l
  |> List.fold_left
       (fun acc b ->
         match (acc, b) with
         | Ok acc, Ok v -> Ok (v :: acc)
         | Ok _acc, Error v -> Error v
         | (Error _ as e), _ -> e )
       (Ok [])
  |> function Ok v -> Ok (List.rev v) | e -> e

let load_sexp label conv file =
  Logs.debug (fun l -> l "Reading file %a for %s" Fpath.pp file label) ;
  OS.File.read file
  >>= fun b ->
  try Sexplib.Sexp.of_string b |> conv |> R.ok with exn ->
    R.error_msg
      (Fmt.strf "Error parsing %a: %s" Fpath.pp file (Printexc.to_string exn))

let save_sexp label conv file v =
  Logs.debug (fun l -> l "Writing file %a for %s" Fpath.pp file label) ;
  let b = Sexplib.Sexp.to_string_hum (conv v) in
  OS.File.write file b

let run_git ~repo args = OS.Cmd.(run Cmd.(v "git" % "-C" % p repo %% args))

let git_ls_remote remote =
  OS.Cmd.(run_out Cmd.(v "git" % "ls-remote" % remote) |> to_lines)
  >>= map (fun l ->
          match String.cuts ~empty:false ~sep:"\t" l with
          | [_; r] -> (
            match String.cuts ~sep:"/" r with
            | ["refs"; "tags"; tag] -> Ok (`Tag tag)
            | ["refs"; "heads"; head] -> Ok (`Head head)
            | _ -> Ok `Other )
          | _ ->
              R.error_msg
                (Fmt.strf "unable to parse git ls-remote for %s: '%s'" remote l)
      )
  >>= fun l ->
  let tags, heads =
    List.fold_left
      (fun (tags, heads) -> function `Other -> (tags, heads)
        | `Tag t -> (t :: tags, heads) | `Head h -> (tags, h :: heads) )
      ([], []) l
  in
  Ok (tags, heads)

let git_local_duniverse_remotes () =
  OS.Cmd.(run_out Cmd.(v "git" % "remote") |> to_lines)
  >>| List.filter (String.is_prefix ~affix:"duniverse-")

let run_opam_package_deps packages =
  let packages = String.concat ~sep:"," packages in
  let cmd =
    let open Cmd in
    v "opam" % "list" % "--color=never" % "-s" % ("--resolve=" ^ packages)
    % "-V" % "--no-switch" % "-S"
  in
  OS.Cmd.(run_out cmd |> to_lines ~trim:true)

let get_opam_field ~field package =
  let field = field ^ ":" in
  let cmd =
    let open Cmd in
    v "opam" % "show" % "--color=never" % "--normalise" % "-f" % field
    % package
  in
  OS.Cmd.(run_out cmd |> to_string ~trim:true)
  >>= fun r ->
  match r with
  | "" -> Ok OpamParserTypes.(List (("", 0, 0), []))
  | r ->
    try Ok (OpamParser.value_from_string r "") with exn ->
      Error (`Msg (Fmt.strf "parsing error for: '%s'" r))

let get_opam_field_string_value ~field package =
  get_opam_field ~field package
  >>= fun v ->
  let open OpamParserTypes in
  match v with
  | String (_, v) -> Ok v
  | List (_, []) -> Ok ""
  | _ ->
      R.error_msg
        (Fmt.strf
           "Unable to parse opam string.\n\
            Try `opam show --normalise -f %s: %s`"
           field package)

let get_opam_dev_repo package =
  get_opam_field_string_value ~field:"dev-repo" package

let get_opam_archive_url package =
  get_opam_field_string_value ~field:"url.src" package
  >>= function "" -> Ok None | uri -> Ok (Some uri)

let get_opam_depends package =
  get_opam_field ~field:"depends" package
  >>= fun v ->
  let open OpamParserTypes in
  match v with
  | List (_, vs) ->
      let ss =
        List.fold_left
          (fun acc -> function String (_, v) -> v :: acc
            | Option (_, String (_, v), _) -> v :: acc | _ -> acc )
          [] vs
      in
      Logs.debug (fun l ->
          l "Depends for %s: %s" package (String.concat ~sep:" " ss) ) ;
      Ok ss
  | _ ->
      R.error_msg
        (Fmt.strf
           "Unable to parse opam depends for %s\n\
            Try `opam show --normalise -f depends: %s` manually"
           package package)

let query_github_repo_exists ~user ~repo =
  let url = Fmt.strf "https://github.com/%s/%s" user repo in
  let cmd = Cmd.(v "curl" % "--silent" % url) in
  OS.Cmd.(run_out cmd |> to_string ~trim:true) >>| fun r -> r <> "Not Found"

(* Currently unused due to API limits *)
let query_github_api ~user ~repo frag =
  let url = Fmt.strf "https://api.github.com/repos/%s/%s/%s" user repo frag in
  let cmd = Cmd.(v "curl" % "--silent" % url) in
  OS.Cmd.(run_out cmd |> to_string ~trim:true) >>| Ezjsonm.from_string

(* Currently unused due to API limits *)
let get_github_branches ~user ~repo =
  query_github_api ~user ~repo "branches"
  >>= fun j ->
  try
    Ok
      Ezjsonm.(
        get_list (fun d -> get_dict d |> List.assoc "name" |> get_string) j)
  with exn ->
    R.error_msg
      (Fmt.strf "Unable to get remote branches for github/%s/%s" user repo)

(* Currently unused due to API limits *)
let get_github_tags ~user ~repo =
  query_github_api ~user ~repo "tags"
  >>= fun j ->
  try
    Ok
      Ezjsonm.(
        get_list (fun d -> get_dict d |> List.assoc "name" |> get_string) j)
  with exn ->
    R.error_msg
      (Fmt.strf "Unable to get remote branches for github/%s/%s: %s" user repo
         Ezjsonm.(to_string (wrap j)))
