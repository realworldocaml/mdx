Mdx can also understand ocaml code blocks:


```ocaml file=parts-begin-end.ml,part=toto,skip
# let x = 3;;
val x : int = 3
# let y = 4;;
val y : int = 4
# print_int x;;
3
- : unit = ()
# print_int y;;
4
- : unit = ()
```

```ocaml file=parts-begin-end.ml,part=z_zz,skip
```

```ocaml file=parts-begin-end.ml,part=4-2,skip
```

```ocaml file=parts-begin-end.ml,skip
```

```ocaml
# let x = 2;;
val x : int = 2
# print_int x;;
3
- : unit = ()
```

```ocaml file=parts-begin-end.ml,part=indented,skip
```
