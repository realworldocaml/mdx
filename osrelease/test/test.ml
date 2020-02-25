let detect_stuff () =
  let os = Osrelease.OS.v () in
  let arch = Osrelease.Arch.v () in
  let distro = Osrelease.Distro.v () |> Rresult.R.get_ok in
  let ver = Osrelease.Version.v () |> Rresult.R.get_ok in
  Printf.printf "%s %s %s %s\n%!"
    (Osrelease.OS.to_opam_string os)
    (Osrelease.Arch.to_opam_string arch)
    (Osrelease.Distro.to_opam_string distro)
    (match ver with None -> "?" | Some v -> v)

let _ = detect_stuff ()
