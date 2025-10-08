Mdx can skip blocks based on `os_type`:

```ocaml os_type<>Win32
# #require "unix"
# ignore (Unix.nice 0)
- : unit = ()
```

```ocaml os_type=Win32
# #require "unix"
# ignore (Unix.nice 0)
Exception: Invalid_argument "Unix.nice not implemented".
```
