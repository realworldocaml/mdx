let sexp =
  let pp fmt s = Fmt.pf fmt "%s" (Mdx.Util.Csexp.to_string s) in
  Alcotest.testable pp ( = )

let msg =
  let pp fs = function `Msg s -> Fmt.pf fs "`Msg %S" s in
  Alcotest.testable pp ( = )

let errormsg v = Alcotest.(result v msg)
let block = Alcotest.testable Mdx.Block.dump ( = )
let header = Alcotest.testable Mdx.Block.Header.pp ( = )
