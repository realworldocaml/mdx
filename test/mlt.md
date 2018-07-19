Mdx can also understand ocaml code blocks:

```ocaml
# #require "fmt";;
# let x = 3;;
val x : int = 3
# x + "foo";;
Characters 4-9:
Error: This expression has type string but an expression was expected of type
         int
```
