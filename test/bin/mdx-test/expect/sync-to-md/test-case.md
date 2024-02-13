Mdx can also understand ocaml code blocks:


```ocaml file=sync_to_md.ml,part=toto,skip
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

```ocaml file=sync_to_md.ml,part=zzz,skip
```

```ocaml file=sync_to_md.ml,part=42,skip
```

```ocaml file=sync_to_md.ml,part=
```

```ocaml file=sync_to_md.ml,skip
```

```ocaml
# let x = 2;;
val x : int = 2
# print_int x;;
3
- : unit = ()
```
