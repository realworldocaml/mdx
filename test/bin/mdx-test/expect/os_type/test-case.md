Mdx can skip blocks based on `os_type`:

```ocaml os_type<>Win32
# #require "unix"
# Unix.nice 0
- : int = 0
```

```ocaml os_type=Win32
# #require "unix"
# Unix.nice 0
Exception: Invalid_argument "Unix.nice not implemented".
```
