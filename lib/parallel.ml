open Stdune

let n_threads = 24

let map ~f l =
  let queue = ref l in
  let queue_lock = Mutex.create () in
  let rec worker result =
    Mutex.lock queue_lock;
    let hd =
      match !queue with
      | [] -> None
      | hd :: tl ->
          queue := tl;
          Some hd
    in
    Mutex.unlock queue_lock;
    match hd with
    | None -> ()
    | Some hd ->
        result := f hd :: !result;
        worker result
  in
  let threads, results =
    List.init n_threads ~f:(fun _ ->
        let result = ref [] in
        (Thread.create worker result, result) )
    |> List.split
  in
  List.iter ~f:Thread.join threads;
  List.map ~f:( ! ) results |> List.flatten
