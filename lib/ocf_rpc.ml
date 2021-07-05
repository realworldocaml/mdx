module Sexp = Sexplib0.Sexp
module Csexp = Csexp.Make (Sexp)
module Ocf = Ocamlformat_rpc_lib
module Result = Util.Result
open Result.Infix

type state = Uninitialized | Running of Ocf.client | Errored

let state : state ref = ref Uninitialized

let start () =
  let err _ =
    state := Errored;
    `No_process
  in
  Result.map_error ~f:err @@ Bos.OS.Env.req_var "MDX__OCAMLFORMAT_RPC_CLIENT"
  >>= fun client_env ->
  Result.map_error ~f:err @@ Csexp.parse_string client_env >>= fun sexp ->
  Result.map_error ~f:err @@ Ocf.client_of_sexp sexp >>= fun client ->
  Ocf.new_session client;
  state := Running client;
  Ok client

let try_format_as_list ?(toplevel = false) l cl =
  let split_lines = Astring.String.cuts ~sep:"\n" ~empty:true in
  let chop_semisemi x =
    match Astring.String.cut ~sep:"\n" x with Some (";;", x) -> x | _ -> x
  in
  let whole = Astring.String.concat ~sep:"\n" l in
  if toplevel then
    match Ocf.format whole cl with
    | exception _ -> l
    | Error _ -> l
    | Ok fmted -> (
        match List.rev (split_lines (chop_semisemi fmted)) with
        | "" :: x :: r | x :: r -> List.rev ((x ^ ";;") :: r)
        | [] -> failwith "command are not empty")
  else
    match Astring.String.cuts ~sep:";;" whole with
    | [] -> []
    | [ x ] -> (
        match Ocf.format x cl with
        | exception _ -> l
        | Error _ -> l
        | Ok fmted -> (
            match List.rev (split_lines fmted) with
            | "" :: r -> List.rev r
            | x :: r -> List.rev (x :: r)
            | [] -> failwith "command are not empty"))
    | phrases ->
        let fmted =
          List.map
            (fun phrase ->
              if Astring.String.(is_empty (trim phrase)) then []
              else
                match Ocf.format phrase cl with
                | exception _ ->
                    Astring.String.cuts ~sep:"\n" ~empty:true (phrase ^ ";;")
                | Error _ ->
                    Astring.String.cuts ~sep:"\n" ~empty:true (phrase ^ ";;")
                | Ok fmted -> (
                    match List.rev (split_lines (chop_semisemi fmted)) with
                    | [ ""; x1 ] -> [ x1 ^ ";;" ]
                    | "" :: x :: r -> List.rev ((x ^ ";;") :: r)
                    | x :: r -> List.rev ((x ^ ";;") :: r)
                    | [] -> failwith "command are not empty"))
            phrases
        in
        List.concat fmted

let get_client () =
  match !state with
  | Uninitialized -> start ()
  | Running cl ->
      let i, _ = Unix.waitpid [ WNOHANG ] (Ocf.pid cl) in
      if i = 0 then Ok cl else start ()
  | Errored -> Error `No_process

let halt () =
  match get_client () with
  | Ok client -> Ocf.end_session client
  | (exception _) | Error _ -> ()

let try_config x =
  match get_client () >>= Ocf.config x with
  | (exception _) | Error _ | Ok () -> ()

let try_format_as_list ?toplevel x =
  match get_client () with
  | (exception _) | Error _ -> x
  | Ok cl -> try_format_as_list ?toplevel x cl
