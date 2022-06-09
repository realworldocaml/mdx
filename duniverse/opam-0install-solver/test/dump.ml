(* For every package name in the opam repository, solve for that package and
   collect the results in a CSV file. *)

(*
module Cached_dir_context = struct
  include Opam_0install.Dir_context

  let cache = Hashtbl.create 10000

  let candidates t name =
    match Hashtbl.find_opt cache name with
    | Some x -> x
    | None ->
      let r = candidates t name in
      Hashtbl.add cache name r;
      r
end

let env =
  Opam_0install.Dir_context.std_env
    ~arch:"x86_64"
    ~os:"linux"
    ~os_family:"debian"
    ~os_distribution:"debian"
    ~os_version:"10"
    ()

module Solver = Opam_0install.Solver.Make(Cached_dir_context)
*)

module Solver = Opam_0install.Solver.Make(Opam_0install.Switch_context)
module Name = OpamPackage.Name

let pp_pkg = Fmt.of_to_string OpamPackage.to_string

let pp_result f = function
  | Ok sels -> Fmt.pf f "%a" Fmt.(list ~sep:(any " ") pp_pkg) (Solver.packages_of_result sels)
  | Error _ -> Fmt.pf f "NO-SOLUTION"

let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid
  with Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_non_intr pid

let dump_slice ~tmpfile ~available ~st proc start finish =
  match Unix.fork () with
  | 0 -> (* We are the child *)
    begin try
        let total = finish - start in
        let ch = open_out tmpfile in
        let f = Format.formatter_of_out_channel ch in
        let start_time = Unix.gettimeofday () in
        for i = start to finish - 1 do
          let off = i - start in
          if off mod 10 = 0 then Fmt.pr "Process %d completed %d/%d packages@." proc off total;
          let name = available.(i) in
          let constraints = OpamPackage.Name.Map.empty in
          let context = Opam_0install.Switch_context.create ~constraints st in
          (* let context = Opam_0install.Dir_context.create ~constraints ~env "/tmp/opam-repository/packages" in *)
          let t0 = Unix.gettimeofday () in
          let r = Solver.solve context [name] in
          let t1 = Unix.gettimeofday () in
          Fmt.pf f "%s, %.4f, %a@." (OpamPackage.Name.to_string name) (t1 -. t0) pp_result r
        done;
        let end_time = Unix.gettimeofday () in
        close_out ch;
        Fmt.pr "Process %d finished (%.2f packages / second)@." proc (float_of_int total /. (end_time -. start_time));
        exit 0
      with ex ->
        print_endline (Printexc.to_string ex);
        exit 1
    end
  | child -> child

let run n_cores root_dir results_file =
  let root_dir = Stdlib.Option.map OpamFilename.Dir.of_string root_dir in
  let t0 = Unix.gettimeofday () in
  OpamClientConfig.opam_init ?root_dir ();
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  let t1 = Unix.gettimeofday () in
  Fmt.pr "Opam library initialised in %.2f s@." (t1 -. t0);
  let available = Lazy.force st.OpamStateTypes.available_packages
                  |> OpamPackage.Set.to_seq
                  |> Seq.map OpamPackage.name
                  |> Name.Set.of_seq |> Name.Set.to_seq
                  |> Array.of_seq in
  (* let available = Array.sub available 0 100 in *)
  let pkgs_per_core = float_of_int (Array.length available) /. float_of_int n_cores |> Float.ceil |> int_of_float in
  let jobs = List.init n_cores (fun i ->
      let start = i * pkgs_per_core in
      let finish = min (start + pkgs_per_core) (Array.length available) in
      let tmpfile = Filename.temp_file "opam-0install-dump-" ".csv" in
      let child = dump_slice ~tmpfile ~available ~st i (i * pkgs_per_core) finish in
      (tmpfile, child)
    ) in
  let wait_for (tmpfile, c) = (tmpfile, waitpid_non_intr c) in
  let t0 = Unix.gettimeofday () in
  let results = List.map wait_for jobs in
  let t1 = Unix.gettimeofday () in
  results |> List.iteri (fun i (_, (_pid, r)) ->
      if r = Unix.WEXITED 0 then Fmt.pr "%d: OK@." i
      else Fmt.pr "%d: failed@." i
    );
  let time = t1 -. t0 in
  Fmt.pr "Finished in %.1f s (%.2f packages / second)@." time (float_of_int (Array.length available) /. time);
  let ch = open_out results_file in
  jobs |> List.iter (fun (tmpfile, _) ->
    let part = open_in tmpfile in
    let len = in_channel_length part in
    let data = really_input_string part len in
    close_in part;
    output_string ch data;
    Unix.unlink tmpfile;
    );
  close_out ch;
  Fmt.pr "Wrote %S@." results_file

open Cmdliner

let output = Arg.(required @@ (pos 0 (some string)) None @@ info ~docv:"OUTPUT.csv" [])

let jobs = Arg.(required @@ (opt (some int)) None @@ info ~docv:"N" ["j"; "jobs"])

let root_dir = Arg.(value @@ (opt (some dir)) None @@ info ~docv:"DIR" ["root"])

let cmd : unit Cmd.t =
  let doc = "solve for every package in a repository" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) performs a solve for every package name in a repository,
        writing the results to a CSV file for analysis.";
  ] in
  let info = Cmd.info "dump" ~doc ~man in
  let term = Term.(const run $ jobs $ root_dir $ output) in
  Cmd.v info term

let () = exit @@ Cmd.eval cmd
