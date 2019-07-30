open Stdune

let n_threads = 24

(** Creates a thread-safe list with a pop function. *)
let protected_list lst =
  let list = ref lst in
  let list_lock = Mutex.create () in
  fun () ->
    Mutex.lock list_lock;
    let hd =
      match !list with
      | [] -> None
      | hd :: tl ->
          list := tl;
          Some hd
    in
    Mutex.unlock list_lock;
    hd

let map ~f l =
  let pop = protected_list l in
  let rec worker result =
    match pop () with
    | None -> ()
    | Some hd ->
        result := f hd :: !result;
        worker result
  in
  List.init n_threads ~f:(fun _ ->
      let result = ref [] in
      (Thread.create worker result, result) )
  |> List.fold_left
       ~f:(fun acc (thread, result_ref) ->
         Thread.join thread;
         !result_ref @ acc )
       ~init:[]
