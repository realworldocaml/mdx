(* Generate a README including the man pages *)

open Rresult
open Bos
open Printf

let rec iter fn l = match l with hd :: tl -> fn hd >>= fun () -> iter fn tl | [] -> Ok ()

let r =
  let files = Sys.argv |> Array.to_list |> List.tl in
  OS.File.read Fpath.(v "header.md") >>= fun header ->
  printf "%s\n\n" header;
  printf "# Manual Pages\n\n";
  iter
    (fun file ->
      OS.File.read Fpath.(v (file ^ ".1")) >>= fun m ->
      printf "## %s\n\n```\n%s\n```\n\n" file m;
      Ok () )
    files
  >>= fun () ->
  printf "%!";
  Ok ()

let () =
  match r with
  | Ok () -> ()
  | Error (`Msg m) ->
      prerr_endline m;
      exit 1
