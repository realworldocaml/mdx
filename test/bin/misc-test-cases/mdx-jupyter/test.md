## Generating a Jupyter notebook

```ocaml
let x = 1
let () = assert true
```

This block should not be executed by the tests:
```ocaml skip
let x = 2
let () = assert false (* Don't print this! *)
```

```ocaml
let () = print_int x
```
