The errors raised when evaluating OCaml blocks are displayed in a `mdx-error` block, that is immediately following the `ocaml` block it is attached to:

```ocaml
module Counter: Irmin.Contents.S with type t = int64 = struct
	type t = int64
	let t = Irmin.Type.int64
```
```mdx-error
Line 4, characters 3-3:
Error: Syntax error: 'end' expected
Line 1, characters 56-62:
  This 'struct' might be unmatched
```

Existing empty errors blocks are filled:

```ocaml
module Counter: Irmin.Contents.S with type t = int64 = struct
	type t = int64
	let t = Irmin.Type.int64
end
```
```mdx-error
```

Or updated:

```ocaml
module Counter: Irmin.Contents.S with type t = int64 = struct
	type t = int64
	let t = Irmin.Type.int64
end
```
```mdx-error
Line 4, characters 3-3:
Error: Syntax error: 'end' expected
Line 1, characters 56-62:
  This 'struct' might be unmatched
```

If no error is raised, no error block must be attached:

```ocaml
module Counter = struct
	type t = int64
end
```

Existing error blocks attached to a valid ocaml block are removed:

```ocaml
module Counter = struct
	type t = int64
end
```
```mdx-error
Line 4, characters 3-3:
Error: Syntax error: 'end' expected
Line 1, characters 56-62:
  This 'struct' might be unmatched
```
