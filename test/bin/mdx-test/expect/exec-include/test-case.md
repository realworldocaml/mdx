.mli files are included but not executed:
<!-- $MDX file=code.mli -->
```ocaml
val f : int -> int
```

.mli files are still included when `skip` is used:
<!-- $MDX file=code.mli,skip -->
```ocaml
val f : int -> int
```

`exec` has no effect:
<!-- $MDX file=code.mli,exec -->
```ocaml
val f : int -> int
```

.ml files are included but not executed by default:
<!-- $MDX file=code.ml,part=OK -->
```ocaml
let f x = x + 1
```

<!-- $MDX file=code.ml,part=KO -->
```ocaml
let k = x = 1
```
```mdx-error
Line 1, characters 9-10:
Error: Unbound value x
```

.ml files are no longer included when `skip` is used:
<!-- $MDX file=code.ml,part=OK,skip -->
```ocaml
let f x = x + 1
```

<!-- $MDX file=code.ml,part=KO,skip -->
```ocaml
let k = x = 1
```

.ml files are executed when `exec` is used:
<!-- $MDX file=code.ml,part=OK,exec -->
```ocaml
let f x = x + 1
```

<!-- $MDX file=code.ml,part=KO,exec -->
```ocaml
let k = x = 1
```
```mdx-error
Line 1, characters 9-10:
Error: Unbound value x
```
