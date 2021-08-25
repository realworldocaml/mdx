open Import

type t = OpamPackage.t * OpamUrl.t

let equal (pkg, url) (pkg', url') = OpamPackage.equal pkg pkg' && url = url'

let pp fmt (pkg, url) =
  Format.fprintf fmt "(%a, %a)" Opam.Pp.package pkg Opam.Pp.url url

let sort_uniq pin_depends =
  let open Result.O in
  let add acc ((pkg, url) as t) =
    let name = OpamPackage.name pkg in
    match OpamPackage.Name.Map.find_opt name acc with
    | None -> Ok (OpamPackage.Name.Map.add name (pkg, url) acc)
    | Some t' when equal t t' -> Ok acc
    | Some (pkg', url') ->
        Rresult.R.error_msgf
          "Package %s is pinned to different versions/url:\n\
          \  - %s: %s\n\
          \  - %s: %s"
          (OpamPackage.Name.to_string name)
          (OpamPackage.to_string pkg)
          (OpamUrl.to_string url)
          (OpamPackage.to_string pkg')
          (OpamUrl.to_string url')
  in
  Result.List.fold_left ~init:OpamPackage.Name.Map.empty ~f:add pin_depends
  >>| fun map -> OpamPackage.Name.Map.values map

let group_by_url t_list =
  List.fold_left
    ~f:(fun acc (pkg, url) ->
      OpamUrl.Map.update url (fun l -> pkg :: l) [ pkg ] acc)
    ~init:OpamUrl.Map.empty t_list
