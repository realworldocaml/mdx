(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

open Lwt.Infix
open Ezjsonm

exception Escape of ((int * int) * (int * int)) * Jsonm.error

let from_stream (stream: string Lwt_stream.t): value Lwt_stream.t =
  let d = Jsonm.decoder `Manual in
  let rec dec () = match Jsonm.decode d with
    | `Lexeme l -> Lwt.return l
    | `Error e  -> Lwt.fail (Escape (Jsonm.decoded_range d, e))
    | `End      -> assert false
    | `Await    ->
      Lwt_stream.get stream >>= function
      | None    -> Lwt.fail (Escape (Jsonm.decoded_range d, (`Expected `Value)))
      | Some str ->
        Jsonm.Manual.src d str 0 (String.length str);
        dec ()
  in
  let rec value v k = match v with
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Null
    | `Bool _
    | `String _
    | `Float _ as v -> k v
    | _ -> assert false
  and value_o v k = match v with
    | `Ae -> k None
    | _   -> value v (fun v -> k (Some v))
  and arr vs k =
    dec () >>= function
    | `Ae -> k (`A (List.rev vs))
    | v   -> value v (fun v -> arr (v :: vs) k)
  and obj ms k =
    dec () >>= function
    | `Oe     -> k (`O (List.rev ms))
    | `Name n -> dec () >>= fun l -> value l (fun v -> obj ((n, v) :: ms) k)
    | _       -> assert false
  in
  let open_stream () =
    dec () >>= function
    | `As -> Lwt.return_unit
    | l   -> Lwt.fail (Escape (Jsonm.decoded_range d, `Expected (`Aval true)))
  in
  let get () =
    dec () >>= fun v ->
    value_o v Lwt.return
  in
  let opened = ref false in
  let open_and_get () =
    if not !opened then (
      open_stream () >>= fun () ->
      opened := true;
      get ()
    ) else
      get ()
  in
  Lwt_stream.from open_and_get
