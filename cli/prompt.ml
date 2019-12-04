let ask f =
  Logs.app (fun l ->
      f (fun ?header ?tags fmt ->
          l ?header ?tags ("%a" ^^ fmt ^^ " [Y/n]") Duniverse_lib.Styled_pp.question_header ()))

let confirm ~question ~yes =
  let rec loop () =
    ask question;
    match String.lowercase_ascii (read_line ()) with
    | "" | "y" | "yes" -> true
    | "n" | "no" -> false
    | _ ->
        Logs.app (fun l ->
            l "Please answer with \"y\" for yes, \"n\" for no or just hit enter for the default");
        loop ()
  in
  if yes then true else loop ()

let confirm_or_abort ~question ~yes =
  if confirm ~question ~yes then Ok () else Error (`Msg "Aborting on user demand")
