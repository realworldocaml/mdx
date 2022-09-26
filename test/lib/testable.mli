val sexp : Mdx.Util.Sexp.t Alcotest.testable
val msg : [ `Msg of string ] Alcotest.testable

val errormsg :
  'a Alcotest.testable -> ('a, [ `Msg of string ]) result Alcotest.testable

val block : Mdx.Block.t Alcotest.testable
val header : Mdx.Block.Header.t Alcotest.testable
