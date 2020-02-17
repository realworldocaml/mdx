module Testable = struct
  include Testable

  let dep =
    let pp fmt dep =
      match (dep : Mdx.Dep.t) with
      | File f -> Fmt.pf fmt "File %S" f
      | Dir d -> Fmt.pf fmt "Dir %S" d
    in
    Alcotest.testable pp (=)
end

let test_of_block =
  let make_test ~block_des ~block ~expected () =
    let test_name = Printf.sprintf "of_block: %S" block_des in
    let test_fun () =
      let actual = Mdx.Dep.of_block block in
      Alcotest.(check (option Testable.dep )) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in

  let block_1 = { Mdx.Block.empty with
      labels = Mdx.Block.labels_of_string "file=toto.ml"
    }
    and block_2 = { Mdx.Block.empty with
      labels = Mdx.Block.labels_of_string "dir=tata/"
    }
    and block_3 = { Mdx.Block.empty with
      labels = Mdx.Block.labels_of_string "dir=tata/,skip"
    }
  in
  [
    make_test ~block_des:"Empty" ~block:Mdx.Block.empty ~expected:None ()
  ; make_test ~block_des:"file:toto.ml" ~block:block_1 ~expected:(Some (File "toto.ml")) ()
  ; make_test ~block_des:"dir:tata/" ~block:block_2 ~expected:(Some (Dir "tata/")) ()
  ; make_test ~block_des:"dir=tata/,skip" ~block:block_3 ~expected:None ()
  ]

let test_of_line =
  let make_test ~line_des ~lines ~expected () =
    let test_name = Printf.sprintf "of_line: %S" line_des in
    let test_fun () =
      let actual = Mdx.Dep.of_lines lines in
      Alcotest.(check (list Testable.dep )) test_name expected actual
    in
    (test_name, `Quick, test_fun)
  in
  let lines = Mdx.of_string Mdx.Normal
  {|
Toto

```ocaml file=tikitaka.ml
```
  |}
  and lines2 = Mdx.of_string Mdx.Normal
  {|
Tata

```ocaml file=tuktuk.ml,skip
```

```cram file=burn.sh
```

```sh dir=ping/
```
  |}
  in
  [
    make_test ~lines
      ~line_des:"block: file=tikitaka.ml"
      ~expected:[File "tikitaka.ml"] ();
    make_test ~lines:lines2
      ~line_des:"skip + file + dir"
      ~expected:[File "burn.sh"; Dir "ping/"] ()
  ]

let test_to_sexp =
  let make_test ~name ~input ~expected () =
    let test_name = Printf.sprintf "to_sexp: %s" name in
    let test_fun () =
      let actual = Mdx.Dep.to_sexp input in
      Alcotest.(check Testable.sexp test_name expected actual)
    in
    (test_name, `Quick, test_fun)
  in
  [ make_test
      ~name:"file"
      ~input:(File "a.ml")
      ~expected:(List [Atom "file"; Atom "a.ml"])
      ()
  ; make_test
      ~name:"dir"
      ~input:(Dir "./a/b")
      ~expected:(List [Atom "dir"; Atom "./a/b"])
      ()
  ]

let suite = ("Dep", test_of_block @ test_of_line @ test_to_sexp)
