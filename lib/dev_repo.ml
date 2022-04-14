open Import

type t = string

let compare = String.compare
let from_string s = s
let to_string t = t

let repo_name t =
  let uri = Uri.of_string t in
  let path = Uri.path uri in
  let last_path_component =
    match String.rsplit2 ~on:'/' path with
    | None -> path
    | Some (_, last_path_component) -> last_path_component
  in
  match String.lsplit2 ~on:'.' last_path_component with
  | None -> last_path_component
  | Some (repo_name, _ext) -> repo_name

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
