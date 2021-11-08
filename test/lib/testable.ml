let r_msg =
  Alcotest.testable Rresult.R.pp_msg (fun (`Msg s) (`Msg s') ->
      String.equal s s')

let sexp = Alcotest.testable Sexplib0.Sexp.pp Sexplib0.Sexp.equal
