https://pandoc.org/MANUAL.html#extension-fenced_code_attributes are parsed and left untouched.
Also make sure we actually parse all of these by having the wrong output to trigger a correction.

```{.sh}
$ echo foo
foo
```

``` {.ocaml}
# let x = 3;;
val x : int = 4
```

```{#identifier .ocaml}
# let x = 3;;
val x : int = 4
```

```{#identifier .ocaml attrib="attrval"}
# let x = 3;;
val x : int = 4
```

```{#identifier .ocaml attrib="attrval with spaces"}
# let x = 3;;
val x : int = 4
```

```{#id1 #id2 .ocaml attrib="attrval" attr2="attrval2}"}
# let x = 3;;
val x : int = 4
```

