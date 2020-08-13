We can suppress the output of signatures:

<!-- $MDX require-package=ppx_deriving.show -->
```ocaml
# #require "ppx_deriving.show"
# type t = int * int [@@deriving show]
val pp : Format.formatter -> t -> unit = <fun>
val show : t -> string = <fun>
```

```ocaml
# module type X = sig
    type t
    val x : t
  end
# module X = struct
    type t = int
    let x = 0
  end
module X : sig type t = int val x : t end
```

The errors cannot be suppressed:

```ocaml
# type t = k;;
...
Error: Unbound type constructor k
```
