open Import

let has_git_extension uri =
  let open Result.O in
  let ext_res =
    let+ path = Fpath.of_string (Uri.path uri) in
    Fpath.get_ext ~multi:true path
  in
  match ext_res with Ok ".git" -> true | Ok _ | Error _ -> false
