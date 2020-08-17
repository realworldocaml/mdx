open Rresult
open Astring

type t = Fpath.t

let local_packages t =
  Bos.OS.Dir.exists t >>= fun exists -> if not exists then Ok String.Map.empty else
  Bos.OS.Dir.contents ~rel:true t
  >>| List.filter (Fpath.has_ext ".opam")
  >>| List.map (fun path -> Fpath.(to_string (rem_ext path), t // path))
  >>| String.Map.of_list
