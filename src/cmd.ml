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

let run_git ~repo args = OS.Cmd.(run Cmd.(v "git" % "-C" % p repo % args))

let run_opam_package_deps package =
  let cmd =
    let open Cmd in
    v "opam" % "list" % "--color=never" % "-s" % ("--resolve=" ^ package)
    % "--all-versions"
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
  >>= function "" -> Ok None | uri -> Ok (Some (Uri.of_string uri))

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
