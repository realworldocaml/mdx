Toplevel blocks are formatted when setting option `--format-code`:

```ocaml
# module X = struct let x = 3 end
# let x = 2 in x + 1
```

Invalid blocks are not formatted:

```ocaml
# let x = 2 in
```

Regular OCaml blocks are not formatted:

```ocaml
let _ = let x = 2 in let y = 3 in x + y

module X = struct let x = 3 end
```
