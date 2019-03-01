Testing lines directives

```ocaml
let f x =
  x+x
  +x
```

And

```ocaml version=4.02
# let f x = x + 1
val f : int -> int = <fun>
# let f y =
  y^"foo"
val f : bytes -> bytes = <fun>
```

```ocaml version>=4.06
# let f x = x + 1
val f : int -> int = <fun>
# let f y =
  y^"foo"
val f : string -> string = <fun>
```

And

```ocaml version=4.02
# let f x = function
  | 0 -> 1
  | n ->
  n + "foo"
Characters 45-50:
Error: This expression has type bytes but an expression was expected of type
         int
```

```ocaml version>=4.06
# let f x = function
  | 0 -> 1
  | n ->
  n + "foo"
Characters 45-50:
Error: This expression has type string but an expression was expected of type
         int
```

Let's go recursive:

```sh
$ ocamlc -pp "ocaml-mdx pp" -impl lines.md
File "lines.md", line 33, characters 6-11:
Error: This expression has type string but an expression was expected of type
         int
[2]
```
