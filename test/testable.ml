let r_msg = Alcotest.testable Rresult.R.pp_msg (fun (`Msg s) (`Msg s') -> String.equal s s')
