Mdx can be used to compile sections of a markdown file. For instance:

```sh
$ ocamlc -pp '$MDX pp -s Hello' -impl section.md
$ ./a.out
42
```
