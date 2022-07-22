It is possible to explicitly state the type of a block using the
`block-type` label, working around language header and content based
inference which can sometime lead to troublesome error messages.

The following blocks use a volontarily misleading language header that would
normally lead to errors if we let MDX infer the type of block based on them.

<!-- $MDX type=toplevel -->
```sh
# 1 + 1;;
```

<!-- $MDX type=ocaml -->
```sh
let x = 2
```

<!-- $MDX type=cram -->
```ocaml
$ echo "boom"
```

The include block type is somewhat redundant with the `file=...` label as
so it is not tested here.
