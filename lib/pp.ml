let plural fmt l = Fmt.using (function _ :: _ :: _ -> "s" | _ -> "") Fmt.string fmt l

module Styled = struct
  let header = Fmt.(styled `Blue (const string "==> "))

  let question_header = Fmt.(styled `Magenta (const string "??? "))

  let header_indent = Fmt.(const string "    ")

  let branch = Fmt.(styled `Cyan string)

  let commit = branch

  let package = Fmt.(styled `Yellow Types.Opam.pp_package)

  let package_name = Fmt.(styled `Yellow string)

  let path fmt path = Fmt.(styled `Cyan Fpath.pp) fmt (Fpath.normalize path)

  let good pp = Fmt.(styled `Green pp)

  let bad pp = Fmt.(styled `Red pp)

  let cached fmt cached = if cached then Fmt.(styled `Green (const string " [CACHED]")) fmt () else ()
end
