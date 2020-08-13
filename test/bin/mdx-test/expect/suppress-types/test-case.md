We can suppress the output of type definitions:

```ocaml
# type t = int;;
# let x = 4;;
val x : t = 4
```

The errors cannot be suppressed:

```ocaml
# type t = k;;
...
Error: Unbound type constructor k
```
