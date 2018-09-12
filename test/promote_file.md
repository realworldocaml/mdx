Mdx can also understand ocaml code blocks:

```ocaml file=promote.ml,part=toto
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

```ocaml file=promote.ml,part=zzz
```

```ocaml file=promote.ml,part=42
```

```ocaml file=promote.ml,part=
```

```ocaml file=promote.ml
```

```ocaml
# print_int x;;
3
- : unit = ()
```
