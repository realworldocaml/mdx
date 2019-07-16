val header : unit Fmt.t

val question_header : unit Fmt.t

val branch : string Fmt.t

val commit : string Fmt.t

val package : Duniverse_lib.Types.Opam.package Fmt.t

val package_name : string Fmt.t

val path : Fpath.t Fmt.t

val good : 'a Fmt.t -> 'a Fmt.t

val bad : 'a Fmt.t -> 'a Fmt.t
