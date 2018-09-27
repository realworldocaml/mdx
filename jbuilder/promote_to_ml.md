Mdx can also understand ocaml code blocks:


```ocaml file=promote_to_ml.ml,part=toto
let x = 34
let f = 42.3
let s = "toto"
let f x u = u x

let () =
  print_int x;
  print_float f
;;
```

```ocaml file=promote_to_ml.ml,part=zzz
let () =
  print_string s
;;
```


```ocaml
# let x = 2;;
val x : int = 2
# print_int x;;
2
- : unit = ()
```
