include ListLabels

let is_empty = function [] -> true | _ -> false

let rec equal eq xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && equal eq xs ys
  | _, _ -> false

let rec find_map l ~f =
  match l with
  | [] -> None
  | x :: l -> ( match f x with None -> find_map l ~f | Some _ as res -> res )

let rev_partition_map =
  let rec loop l accl accr ~f =
    match l with
    | [] -> (accl, accr)
    | x :: l -> (
        match (f x : (_, _) Either.t) with
        | Left y -> loop l (y :: accl) accr ~f
        | Right y -> loop l accl (y :: accr) ~f )
  in
  fun l ~f -> loop l [] [] ~f

let partition_map l ~f =
  let l, r = rev_partition_map l ~f in
  (rev l, rev r)

let filter_opt l = filter_map ~f:(fun x -> x) l
