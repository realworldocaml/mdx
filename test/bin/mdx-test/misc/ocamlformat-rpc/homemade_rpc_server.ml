module Result = struct
  module Infix = struct
    let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

    let ( >>| ) r f = match r with Ok x -> Ok (f x) | Error _ as e -> e
  end

  let map_error ~f = function Ok x -> Ok x | Error x -> Error (f x)
end

open Result.Infix
module Ocf = Ocamlformat_rpc_lib

let log = Format.printf

let supported_versions = ["v1"]

type state = Uninitialized | Running of Ocf.V1.Client.t | Errored

let state : state ref = ref Uninitialized

let start () =
  let cmd = Bos.Cmd.v "ocamlformat-rpc" in
  let argv = [|"ocamlformat-rpc"|] in
  ( match
      Bos.OS.Cmd.get_tool cmd >>= fun cmd_path ->
      let prog = Fpath.to_string cmd_path in
      let input, output = Unix.open_process_args prog argv in
      let pid = Unix.process_pid (input, output) in
      let client = Ocf.V1.Client.mk ~pid input output in
      state := Running client ;
      Ocf.V1.Client.config ["profile", "janestreet"] client >>| fun () ->
      client
    with
  | exception _ ->
      Error
        (`Msg
          "OCamlFormat-RPC did not respond. Check that a compatible version \
           of the OCamlFormat RPC server (ocamlformat-rpc >= 0.18.0) is \
           installed." )
  | x -> x )
  |> Result.map_error ~f:(fun (`Msg msg) ->
         state := Errored ;
         log
           "An error occured while initializing and configuring ocamlformat:\n\
            %s\n\
            %!"
           msg ;
         `No_process )

let () =
  match start () with
  | Error `No_process -> exit 1
  | Ok client -> (
      let rec loop () =
        let cmd = Ocf.V1.Command.read_input stdin in
        let out = Ocf.V1.Client.query cmd client in
        Ocf.V1.Command.output stdout out ;
        match out with
        | `Halt -> ()
        | _ -> loop ()
      in
      loop ()
    )
