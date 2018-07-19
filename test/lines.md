Testing lines directives

```ocaml
let f x =
  x+x
  +x
```

And

```ocaml
# let f x = x + 1
val f : int -> int = <fun>
# let f y =
  y^"foo"
val f : string -> string = <fun>
```

And

```ocaml
# let f x = function
  | 0 -> 1
  | n ->
  n + "foo"
Characters 39-44:
Error: This expression has type string but an expression was expected of type
         int
```

Let's go recursive:

```sh
$ ocamlc -pp "mdx pp" -impl lines.md
File "lines.md", line 25, characters 6-11:
Error: This expression has type string but an expression was expected of type
         int
[2]
```
