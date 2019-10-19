Normal OCaml code:

```ocaml
let x = 3

let f x = x + 1

let () = Printf.printf "n: %d\n%!" (f 42)
```

Yo!

```ocaml
# let x = 3
val x : int = 3
# type t = int
type t = int
```

```ocaml
class istack = object end
```

```ocaml
# module type Foo = sig type t end
module type Foo = sig type t end
```


```ocaml skip
# Pipe.f ()
- : unit
```

```ocaml version>=4.08
let (let*) a f = f a
let test ma f =
  let* s = ma in
  f s
```
