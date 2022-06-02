let r_msg =
  Alcotest.testable Rresult.R.pp_msg (fun (`Msg s) (`Msg s') ->
      String.equal s s')

let sexp = Alcotest.testable Sexplib0.Sexp.pp Sexplib0.Sexp.equal

let opam_package_name_set =
  Alcotest.testable Duniverse_lib.Opam.Pp.Package_name_set.pp
    OpamPackage.Name.Set.equal
