open Result

module Result = struct
  module List = struct
    open Rresult.R.Infix

    let fold ~f ~init l =
      let rec go acc = function
        | [] -> Ok acc
        | hd::tl ->
          f acc hd >>= fun acc ->
          go acc tl
      in
      go init l

    let map ~f l =
      fold ~f:(fun acc elm -> f elm >>| fun elm' -> elm'::acc) ~init:[] l >>| List.rev
  end
end
