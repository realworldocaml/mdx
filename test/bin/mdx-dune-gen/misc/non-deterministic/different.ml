(* small helper to determine whether two files differ *)

let main () =
  let first = Sys.argv.(1) |> open_in in
  let second = Sys.argv.(2) |> open_in in
  let rec loop () =
    match input_line first with
    | first_line -> (
        match input_line second with
        | second_line ->
            match String.equal first_line second_line with
            | true -> loop ()
            | false ->
                (* we found a difference between the lines *)
                exit 0
        | exception End_of_file ->
            (* the second file ended before the first *)
            exit 0
    )
    | exception End_of_file ->
        (* the first file ended first *)
        match input_line second with
        | _ ->
            (* the second file continues: a difference *)
            exit 1
        | exception End_of_file ->
            (* the second file ended too *)
            ()
  in
  loop ();
  close_in first;
  close_in second;
  (* we didn't find a difference, exit with a failure code *)
  prerr_endline "The files appear to be identical";
  exit 1

let () =
  main ()
