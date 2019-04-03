Environment variable can also loaded in an environment

```ocaml var=FOO=bar
  # print_endline (Sys.getenv "FOO")
  bar
  - : unit = ()
```

And the variable stays available in subsequent blocks

```ocaml
  # print_endline (Sys.getenv "FOO")
  bar
  - : unit = ()
```
