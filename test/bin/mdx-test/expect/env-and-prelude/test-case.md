Different environments can have different preludes

This is run with `--prelude default.ml --prelude a:a.ml --prelude b:b.ml`.
Each of them defines a `y` variable, respectively set to `10`, `20` and `30`.

```ocaml
# let x = 1
val x : int = 1
```

```ocaml env=a
# let x = 2
val x : int = 2
```

```ocaml env=b
# let x = 3
val x : int = 3
```

```ocaml
# x + y
- : int = 11
# z
- : int = 0
```

```ocaml env=a
# x + y
- : int = 22
# z
...
Error: Unbound value z
```

```ocaml env=b
# x + y
- : int = 33
# z
...
Error: Unbound value z
```

Changing the order of the options on the command line results in weird behaviour.
For instance moving `--prelude default.ml` in the last position triggers the following:
```
``ocaml
 # x + y
-- : int = 31
+Line 1, characters 5-6:
+Error: Unbound value y
 ``
```
