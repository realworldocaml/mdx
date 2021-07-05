module Result = struct
  module Infix = struct
    let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

    let ( >>| ) r f = match r with Ok x -> Ok (f x) | Error _ as e -> e
  end

  let handle_error = function
    | Ok () -> ()
    | Error (`Msg e) -> Format.printf "%s\n%!" e
end

open Result.Infix
module Ocf = Ocamlformat_rpc_lib
module Sexp = Sexplib0.Sexp
module Csexp = Csexp.Make (Sexp)

let supported_versions = ["v1"]

let open_process prog argv =
  match Unix.open_process_args prog argv with
  | exception _ ->
    Error
      (`Msg
         "OCamlFormat-RPC did not respond. Check that a compatible version \
          of the OCamlFormat RPC server (ocamlformat-rpc >= 0.18.0) is \
          installed." )
  | (input, output) -> Ok (input, output)

let log = Format.printf

let () =
  let cmd = Bos.Cmd.v "ocamlformat-rpc" in
  let argv = [|"ocamlformat-rpc"|] in
  let env_var = "MDX__OCAMLFORMAT_RPC_CLIENT" in
  Result.handle_error
  @@
  ( log "Looking for ocamlformat-rpc binary in path...%!";
    Bos.OS.Cmd.get_tool cmd >>= fun cmd_path ->
    log " %a\n%!" Fpath.pp cmd_path;
    let prog = Fpath.to_string cmd_path in
    log "Starting process...%!";
    open_process prog argv >>= fun (input, output) ->
    log " OK\n%!";
    let pid = Unix.process_pid (input, output) in
    log "Selecting ocamlformat-rpc client...%!";
    Ocf.pick_client ~pid input output supported_versions >>= fun client ->
    ( match client with
    | `V1 _ -> log " V1 selected\n%!");
    log "Selecting Janestreet profile...%!";
    Ocf.config [ "profile", "janestreet" ] client >>= fun () ->
    log " OK\n%!";
    let sexp = Ocf.sexp_of_client client in
    let client_env = Csexp.to_string sexp in
    log "Setting %s value...%!" env_var;
    Bos.OS.Env.set_var env_var (Some client_env) >>= fun () ->
    log " OK\n%!";
    Ok () )
