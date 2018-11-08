Long lines can be replaced by ellipsis:


```sh
$ for i in `seq 1 10`; do echo $i; done
1
2
...
10
```

```ocaml
# for i = 1 to 10 do Printf.printf "%d\n%!" i; done
1
2
...
10
- : unit = ()
```

```sh
$ echo "foo\"\n\nbar"
foo"

...
```
