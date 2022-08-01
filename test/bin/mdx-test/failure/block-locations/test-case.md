There was a bug in a the lexer/parser for markdown and cram files
that lead to increasingly big offsets in block locations.
This impacted error reporting quite badly.

Here we have a few valid blocks followed by one that triggers an error,
it should be reported with the correct block location.


<!-- $MDX toplevel -->
```ocaml
# 1 + 1;;
```

<!-- $MDX toplevel -->
```ocaml
# 1 + 1;;
```

<!-- $MDX toplevel -->
```ocaml
# 1 + 1;;
```

<!-- $MDX pif=paf -->
```ocaml
# 1 + 1;;
```
