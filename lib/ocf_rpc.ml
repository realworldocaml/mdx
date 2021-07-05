module Sexp = Sexplib0.Sexp
module Csexp = Csexp.Make (Sexp)
module Ocf = Ocamlformat_rpc_lib
module Result = Util.Result
open Result.Infix

let supported_versions = [ "v1" ]

type state = Uninitialized | Running of Ocf.client | Errored

let state : state ref = ref Uninitialized

let err (`Msg _) =
  state := Errored;
  `No_process

let parse_config s =
  let opts = String.split_on_char ',' s in
  List.fold_left
    (fun acc x ->
      match String.split_on_char '=' x with
      | [ var; val_ ] -> (var, val_) :: acc
      | _ -> acc)
    [] opts

let set_config client =
  let s = Bos.OS.Env.opt_var ~absent:"" "MDX__OCAMLFORMAT_RPC_CONFIG" in
  match Ocf.config (parse_config s) client with
  | (exception _) | Ok () | Error _ -> ()

let start () =
  let argv = [| "ocamlformat-rpc-server" |] in
  let cmd = Bos.Cmd.v "ocamlformat-rpc-server" in
  Bos.OS.Cmd.get_tool cmd >>= fun cmd_path ->
  let prog = Fpath.to_string cmd_path in
  let input, output = Unix.open_process_args prog argv in
  let pid = Unix.process_pid (input, output) in
  Result.map_error ~f:err
  @@ Ocf.pick_client ~pid input output supported_versions
  >>| fun client ->
  state := Running client;
  set_config client;
  client

let try_format_as_list ?(toplevel = false) l cl =
  let split_lines = Astring.String.cuts ~sep:"\n" ~empty:true in
  let add_semisemi x =
    let len = String.length x in
    match (String.get x (len - 1), String.get x (len - 2)) with
    | ';', ';' -> x
    | _ -> x ^ ";;"
  in
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
        | "" :: x :: r | x :: r -> List.rev (add_semisemi x :: r)
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
                    Astring.String.cuts ~sep:"\n" ~empty:true
                      (add_semisemi phrase)
                | Error _ ->
                    Astring.String.cuts ~sep:"\n" ~empty:true
                      (add_semisemi phrase)
                | Ok fmted -> (
                    match List.rev (split_lines (chop_semisemi fmted)) with
                    | [ ""; x1 ] -> [ add_semisemi x1 ]
                    | "" :: x :: r -> List.rev (add_semisemi x :: r)
                    | x :: r -> List.rev (add_semisemi x :: r)
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
  match get_client () >>= Ocf.halt with (exception _) | Error _ | Ok () -> ()

let try_config x =
  match get_client () >>= Ocf.config x with
  | (exception _) | Error _ | Ok () -> ()

let try_format_as_list ?toplevel x =
  match get_client () with
  | (exception _) | Error _ -> x
  | Ok cl -> try_format_as_list ?toplevel x cl
