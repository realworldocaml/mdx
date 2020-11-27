(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module A = Alcotest

let _ =
  let tests =
    Oldpos.tests @ Fullpos.tests
    |> List.map (fun (n,t) ->
        n, List.map (fun (n,t) -> n, `Quick, t) t)
  in
  A.run "opam-file-format" tests
