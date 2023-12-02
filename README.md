[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Frealworldocaml%2Fmdx%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/realworldocaml/mdx)

# MDX

MDX is a tool that generates evaluation results for code interactions inside Markdown (`.md`) 
and `.ml{i,d}` documentation.

MDX is released on opam and can be installed by running:

```sh
$ opam install mdx
```

If you want to contribute to the project, please see the
[CONTRIBUTING.md](CONTRIBUTING.md).

## Setup

MDX is integrated with Dune. To enable MDX for your Dune project, 
you must first add a [`dune-project` stanza](https://dune.readthedocs.io/en/latest/dune-files.html#mdx) to allow Dune to recognize MDX:

```
(using mdx 0.4)
```

See [compatibility](#compatibility) for more details about version compatibility.

Then add the following MDX stanza in relevant `dune` files:

```
(mdx (files :standard *.mli))
```

Under a folder with a `dune` file, `:standard` enables MDX on all Markdown files (`*.md`)
and odoc's documentation pages (`*.mld`); `*.mli` enables MDX on the OCaml interface files.
The MDX stanza can be further configured. Please visit the relevant section of
[Dune's manual](https://dune.readthedocs.io/en/latest/dune-files.html#mdx) and 
the rest of this README for more information.

### Compatibility

- `(using mdx 0.1)` requires Dune 2.4 or higher, with no restriction on MDX version.
- `(using mdx 0.2)` requires Dune 3.0 or higher and MDX 1.9.0 or higher.
- `(using mdx 0.3)` requires TODO
- `(using mdx 0.4)` requires Dune 3.8 or higher and MDX 2.3.0 or higher.

## Getting Started

MDX ensures that your code examples behave the way you expect by actually running them.

MDX supports various types of code blocks but the most common ones are 
_OCaml interactive toplevel blocks_. 
We illustrate one in the example below. In a Markdown file, we may have:

````markdown
Let's look at how good OCaml is with integers and floats:
```ocaml
# 1 + 2;;
- : int = 2
# 1.2 + 3.4;;
- : float = 4.6
```
````
or in an `mli` file:
```ocaml
(** Let's look at how good OCaml is with integers and floats:
    {@ocaml[
    # 1 + 2;;
    - : int = 2
    # 1.2 + 3.4;;
    - : float = 4.6
    ]}
*)
```

The content of the interactive toplevel blocks looks just like an interactive toplevel
session. _Phrases_, i.e., the toplevel "input", start with a `#` and end with `;;`.
The toplevel evaluation, or "output" follows each phrase.

Now you probably have noticed that `1 + 2` is not equal to `2` nor is `1.2 + 3.4` type-checked. 
Somebody must have introduced a typo, or updated the phrases but then forgot to update the evaluation result.

That's exactly why MDX is here!

If we run `dune runtest`, this would be the result:

````sh
$ dune runtest
File "README.md", line 1, characters 0-0:
diff --git a/_build/default/README.md b/_build/default/.mdx/README.md.corrected
index e74437a..dfeeb64 100644
--- a/_build/default/README.md
+++ b/_build/default/.mdx/README.md.corrected
@@ -1,7 +1,9 @@
 Let's look at how good OCaml is with integers and floats:
 ```ocaml
 # 1 + 2;;
-- : int = 2
+- : int = 3
 # 1.2 + 3.4;;
-- : float = 4.6
+Line 1, characters 1-4:
+Error: This expression has type float but an expression was expected of type
+         int
 ```
````

The test run just failed, and Dune showed the diff between what we have
locally and what we should have, according to MDX.
This uses Dune's promotion workflow so that at this point we can investigate
it further if we are surprised by this diff. In this case, we may want to fix 
the second phrase to `# 1.2 +. 3.4;;`. Then, we can re-run `dune runtest` again. 

````sh
$ dune runtest
File "README.md", line 1, characters 0-0:
diff --git a/_build/default/README.md b/_build/default/.mdx/README.md.corrected
index 760debc..8e5878b 100644
--- a/_build/default/README.md
+++ b/_build/default/.mdx/README.md.corrected
@@ -1,7 +1,7 @@
 Let's look at how good OCaml is with integers and floats:
 ```ocaml
 # 1 + 2;;
-- : int = 2
+- : int = 3
 # 1.2 +. 3.4;;
 - : float = 4.6
 ```
````

Since we are now happy with the diff, we can accept it by running:

```
dune promote
```

Now the documentation is up-to-date with the content:

````markdown
Let's look at how good OCaml is with integers and floats:
```ocaml
# 1 + 2;;
- : int = 3
# 1.2 +. 3.4;;
- : float = 4.6
```
````

and running `dune runtest` again should be successful!

## Supported Extensions

### Labels

The blocks can be parameterized by `mdx`-specific labels, that
will change the way `mdx` interprets the block.

The markdown syntax is: `<!-- $MDX LABELS -->`, where `LABELS` is a list of
valid labels separated by a comma. This line has to immediately precede the
block it is attached to.

    <!-- $MDX LABELS -->
    ```ocaml
    ```

The `.mli` and `.mld` syntax for this is is slightly different to match the conventions of
OCaml documentation comments:

    (** This is an documentation comment with an ocaml block
        {@ocaml LABELS [
        ]}
    *)

The possible labels are:

- `skip` -- ignore this block
- `ocaml`, `cram`, `toplevel`, `include` -- set the block type
- `version=VERSION` -- set OCaml version
- `non-deterministic[=output|command]` -- see "Non-deterministic tests" section
- `dir=PATH` -- set the directory where the tests should be run
- `source-tree=PATH` -- does nothing?
- `file=PATH` -- see the "File sync" section
- `part=PART` -- see the "File sync" section
- `env=ENV` -- see the "Named execution environments" section
- `set-VAR=VALUE` -- set an environment variable
- `unset-VAR` -- unset an environment variable

### Shell Scripts

`ocaml-mdx` interprets shell scripts inside `sh` code blocks as cram-like tests. The
syntax is the following:

- Lines beginning with a dollar sign and a space are
  *commands* and will be run in the shell.
- Multi-lines commands end by `\` and continue with two spaces and
  a `>` sign on the next line:

       ```sh
       $ <line1> \
       > <line2> \
       > <line3>
       ```
- Commands support the heredoc syntax (`<<`):

       ```sh
       $ cat <<EOF \
       > hello\
       > world\
       > EOF
       hello
       world
       ```
- Lines beginning without a dollar sign are considered command *outputs*.
- Command outputs can contain *ellipses*: `...`. These will
  match any possible outputs (on zero, one or multiple lines).
- Arbitrary padding with whitespace is supported, as long as it is consistent
  inside a code block.

Here is an example of a markdown file using shell scripts inside code blocks,
with a padding of 3:

    ```sh
       $ for i in `seq 1 10`
       1
       ...
       10
    ```

MDX will also consider exit codes when the syntax `[<exit code>]`is used:

    ```sh
    $ exit 1
    [1]
    ```

Note that nothing will be displayed when the exit code is 0 (e.g. in case
of success).

### OCaml Code

MDX interprets OCaml fragments. It understands _normal_ code fragments and
_toplevel_ code fragments (starting with a `#` sign and optionally ending with
`;;`). Arbitrary whitespace padding is supported, at long as it stays
consistent within a code block.

Toplevel fragments interleave OCaml code and their corresponding outputs.

Here is an example of normal OCaml code:

    ```ocaml
    print_endline "42"
    ```

Here is an examples of toplevel OCaml code:

    ```ocaml
    # print_endline "42"
    42
    ```

## File sync

MDX is also capable of including content from files in fenced code blocks
using the label `file`. OCaml files can be sliced using named blocks:

```ocaml
(* $MDX part-begin=partName *)
let meaning_of_life () =
  print_endline "42"
(* $MDX part-end *)
```

These can then be included in the document:

    <!-- $MDX file=sync_to_md.ml,part=partName -->
    ```ocaml
    ```

Non-OCaml files can also be read and included in a block:

    <!-- $MDX file=any_file.txt -->
    ```
    ```
However, part splitting is only supported for OCaml files.

## Tests

### Cram Tests

Cram tests can be executed and checked with `ocaml-mdx test <file.md>`.

    ```sh
     $ for i in `seq 1 10`; do echo $i; done
     1
     ...
     10
     ```

If the output is not consistent with what is expected,
`<file.md>.corrected` is generated.

### OCaml

To execute OCaml code and toplevel fragments, uses `ocaml-mdx test <file.md>`.

    ```ocaml
    # print_endline "42"
    42
    ```

If the output is not consistent with what is expected
`<file.md>.corrected` is generated.

### Non-deterministic Tests

#### Non-deterministic Outputs

`ocaml-mdx test` supports non-deterministic outputs:

    <!-- $MDX non-deterministic=output -->
    ```sh
    $ <command>
    <output>
    ```

In that case, `ppx test <file>` will run the command but will not
generate `<file>.corrected` if the new output differs from the one
described in the file. Use `ocaml-mdx test --non-deterministic <file>` to come
back to the default behaviour.

#### Non-deterministic Commands

`ocaml-mdx test` supports non-deterministic commands:

    <!-- $MDX non-deterministic=command -->
    ```ocaml
    # Random.int 10;;
    - : int = 5
    ```

In that case, `ocaml-mdx test <file>` will *not* run the command. Use `ocaml-mdx test
--non-deterministic <file>` to come back to the default behaviour.

Alternatively, instead of passing the option it is also possible to set the
environment variable `MDX_RUN_NON_DETERMINISTIC` to make MDX execute
non-deterministic blocks. This is useful when not calling MDX directly but
through other commands like `dune` or Makefiles etc. Use
`MDX_RUN_NON_DETERMINISTIC=1 ocaml-mdx test` in this case.

### Named execution environments (since mdx 1.1.0)

Separate environments can be defined for blocks:

`x` holds the value `1` in the environment `e1`.

    <!-- $MDX env=e1 -->
    ```ocaml
    let x = 1;;
    ```

    <!-- $MDX env=e1 -->
    ```ocaml
    module M = struct let k = 42 let f x = x * k end;;
    ```

`x` holds the value `3` in the environment `e2`.

    <!-- $MDX env=e2 -->
    ```ocaml
    let x = 3;;
    ```

We can retrieve the value of `x` in environment `e1`:

    <!-- $MDX env=e1 -->
    ```ocaml
    # print_int x;;
    1
    - : unit = ()
    # print_int M.k;;
    42
    - : unit = ()
    # M.f;;
    - : int -> int = <fun>
    ```

### Matching on the OCaml version (since mdx 1.2.0)

Blocks can be processed or ignored depending on the current version of OCaml.

For example to have a different outcome whether we are past OCaml 4.06:

    <!-- $MDX version<4.06 -->
    ```ocaml
    # let f x = x + 1
    val f : int -> int = <fun>
    # let f y =
      y^"foo"
    val f : bytes -> bytes = <fun>
    ```

    <!-- $MDX version>=4.06 -->
    ```ocaml
    # let f x = x + 1
    val f : int -> int = <fun>
    # let f y =
      y^"foo"
    val f : string -> string = <fun>
    ```

The available operators are `<>`, `>=`, `>`, `<=`, `<` and `=`.
The version number can be of the following forms:
- `*`
- `X`
- `X.Y`
- `X.Y.Z`

### Matching based on the `os_type` (since mdx 2.4.0)

Block can be processed or ignored depending on the current
[`os_type`](https://v2.ocaml.org/api/Sys.html#VALos_type).

For example, different blocks could be enabled depending on whether we are on
Windows or not:

    ```ocaml
    #require "unix"
    ```

    <!-- $MDX os_type<>Win32 -->
    ```ocaml
    # Unix.nice 0
    - : int = 0
    ```

    <!-- $MDX os_type=Win32 -->
    ```ocaml
    # Unix.nice 0
    Exception: Invalid_argument "Unix.nice not implemented".
    ```

The `os_type` values should be written in ASCII and are compared case
insensitively.

### Environment variables declaration

Environment variables can be declared at the beginning of a block:

    <!-- $MDX set-FOO=bar,set-BAR=foo -->
    ```ocaml
    # print_endline (Sys.getenv "FOO")
    bar
    - : unit = ()
    # print_endline (Sys.getenv "BAR")
    foo
    - : unit = ()
    ```

Those variables are then available in the subsequent blocks

    ```ocaml
    # print_endline (Sys.getenv "FOO")
    bar
    - : unit = ()
    ```
