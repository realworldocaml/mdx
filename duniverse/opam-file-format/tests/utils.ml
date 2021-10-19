(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)


(* Common test functions *)

let add_after_first ?(cond=fun _ -> true) c sep str =
  try
    let length = String.length str in
    if not (cond str) then str else
    let split = String.index str c in
    let fst = String.sub str 0 (split+1) in
    let snd = String.sub str (split+1) (length - split -1) in
    fst ^ sep ^ snd
  with _ -> failwith (Printf.sprintf "error with '%c' on %s" c str)

let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if String.get s i = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r
