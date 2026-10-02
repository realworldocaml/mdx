The `mdx_skip` doesn't offer corrections for its own output
or for later tests in the same block:

```ocaml
# mdx_skip "This test can never work";;
- : unit = ()

# failwith "Shouldn't get here";;
```

`mdx_skip` prevents things after it from being executed at all:

```ocaml
# mdx_skip "Skip the sleep"; Unix.sleep 1000;;
- : unit = ()

# Unix.sleep 1000;;;
```

It also works in plain `ocaml` blocks:

```ocaml
let () =
  mdx_skip "Test wouldn't pass";
  failwith "Shouldn't get here";;
```
```mdx-error
Exception: Failure "Expected".
```

And for non-deterministic blocks:

```ocaml non-deterministic=output
# Random.int 42;;
- : int = 0

# mdx_skip "Skip the sleep";;

# Unix.sleep 100;;
```

This test requires `/bin/cp` and so won't work on NixOS:

```ocaml
let get_cp_path () =
  let path = "/bin/cp" in
  if Sys.file_exists path then path
  else mdx_skip ("Test requires " ^ path)
```

```ocaml
# (Unix.stat (get_cp_path ())).st_kind;;
- : Unix.file_kind = Unix.S_REG
```

The example in the README:

```ocaml
# if Sys.word_size < 64 then mdx_skip "Requires 64-bit words";;
- : unit = ()

# 0x100000000;;
- : int = 4294967296
```

Named environments also allow skipping:

<!-- $MDX env=e1 -->
```ocaml
# mdx_skip "Named environment";;
```
