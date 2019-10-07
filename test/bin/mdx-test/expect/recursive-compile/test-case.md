Mdx can be used to compile sections of a markdown file. For instance:

```sh
$ ocamlc -pp 'ocaml-mdx pp -s Hello' -impl ./recursive_compile.md.input
$ ./a.out
42
```

OCaml toplevel can also be compiled and executed in the same way:


```sh
$ ocamlc -pp 'ocaml-mdx pp -s Toplevel' -impl ./recursive_compile.md.input && ./a.out
42
```
