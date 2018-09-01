We can define separate environments for blocks.

`x` holds the value `1` in the environment `e1`.

```ocaml env=e1
let x = 1;;
```

```ocaml env=e1
module M = struct let k = 42 let f x = x * k end;;
```

`x` holds the value `3` in the environment `e2`.

```ocaml env=e2
let x = 3;;
```

We can retrieve the value of `x` in environment `e1`:

```ocaml env=e1
# print_int x;;
1
- : unit = ()
# print_int M.k;;
42
- : unit = ()
# M.f;;
- : int -> int = <fun>
```
