# Using local library

```ocaml require=example_lib
# #require "example_lib"
# Example_lib.hello ()
Hello world!
- : unit = ()
```

# Sub library

```ocaml require=example_lib.b
# #require "example_lib.b"
# Lib_b.hello ()
Hello world!
- : unit = ()
```

# Executables

```sh require=example_lib.exe
$ example_lib.exe Hello
Hello world!
```
