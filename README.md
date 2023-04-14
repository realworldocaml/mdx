[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Frealworldocaml%2Fmdx%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/realworldocaml/mdx)

## MDX

MDX allows to execute code blocks inside markdown and mli/mld documentation
to help keeping them up to date.

Use the
[dune stanza](https://dune.readthedocs.io/en/latest/dune-files.html#mdx-since-2-4)
to enable it on your documentation.

`mdx` is released on opam and can be installed by running:

```sh
$ opam install mdx
```

If you want to contribute to the project, please see the
[CONTRIBUTING.md](CONTRIBUTING.md).

### Basic Usage

You can use MDX with your Markdown or `.ml{i,d}` documentation, which ensures
code in multi-line or verbatim code blocks is correct.

To enable MDX on specific files you must first enable it for your project by
adding the following stanza to your `dune-project`:
```
(using mdx 0.2)
```

Note that version `0.2` of the MDX stanza is only available in dune `3.0` or
higher. You can use the first, `0.1` version with dune `2.4` or higher.

Then add the following in the relevant `dune` file:
```
(mdx)
```
That enables MDX on all markdown files in the folder.
The MDX stanza can be further configured. Please visit the relevant section of
[dune's manual](https://dune.readthedocs.io/en/latest/dune-files.html#mdx-since-2-4)
for more information.

MDX supports various type of code blocks but the most common are OCaml toplevel
blocks. We illustrate one in our example below. In a Markdown file, you
would write something similar to this:

````markdown
Let's look at how good OCaml is with integers and strings:
```ocaml
# 1 + 2;;
- : int = 2
# "a" ^ "bc";;
- : string = "ab"
```
````
or in an `mli` file:
```ocaml
(** Let's look at how good OCaml is with integers and strings:
    {@ocaml[
    # 1 + 2;;
    - : int = 2
    # "a" ^ "bc";;
    - : string = "ab"
    ]}
*)
```

The content of the toplevel blocks looks just like an interactive toplevel
session. Phrases, i.e., the toplevel "input", start with a `#` and end with `;;`.
The toplevel evaluation, or "output" follows each phrase.

Now you probably have noticed that `1 + 2` is not equal to `2` nor is `"a" ^ "bc"`
to `"ab"`. Somebody must have updated the phrases, but then forgot to update
the evaluation.

That's exactly why MDX is here!

If you enable MDX for this file and then ran `dune runtest`, this would be the
result:

````
$ dune runtest
File "README.md", line 1, characters 0-0:
       git (internal) (exit 1)
(cd _build/default && /usr/bin/git --no-pager diff --no-index --color=always -u README.md .mdx/README.md.corrected)
diff --git a/README.md b/.mdx/README.md.corrected
index 181b86f..458ecec 100644
--- a/README.md
+++ b/.mdx/README.md.corrected
@@ -1,13 +1,13 @@
Let's look at how good OCaml is with integers and strings:
```ocaml
# 1 + 2;;
-- : int = 2
+- : int = 3
# "a" ^ "bc";;
-- : string = "ab"
+- : string = "abc"
```
````

The test run just failed and dune is showing the diff between what we have
locally and what should be, according to MDX.
This uses dune's promotion workflow so at this point you can either investigate
it further if you're surprised by this diff or if you're happy with it, simply
accept it by running:

```
dune promote
```

Now the documentation is up-to-date and running `dune runtest` again should be
successful!

Note that to use the `dune runtest/promote` workflow with `mli` or `mld` files,
you will need to adjust the `mdx` stanza in the `dune` file, as by
[default](https://dune.readthedocs.io/en/latest/dune-files.html#mdx-since-2-4),
Dune only checks markdown files with `mdx`.  E.g.,

```
(mdx
 (files :standard - *.mli))
```

### Supported Extensions

#### Labels

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

#### Shell Scripts

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

#### OCaml Code

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

### File sync

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

### Tests

#### Cram Tests

Cram tests can be executed and checked with `ocaml-mdx test <file.md>`.

    ```sh
     $ for i in `seq 1 10`; do echo $i; done
     1
     ...
     10
     ```

If the output is not consistent with what is expected,
`<file.md>.corrected` is generated.

#### OCaml

To execute OCaml code and toplevel fragments, uses `ocaml-mdx test <file.md>`.

    ```ocaml
    # print_endline "42"
    42
    ```

If the output is not consistent with what is expected
`<file.md>.corrected` is generated.

#### Non-deterministic Tests

**Non-deterministic Outputs**

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

**Non-deterministic Commands**

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

#### Named execution environments (since mdx 1.1.0)

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

#### Matching on the OCaml version (since mdx 1.2.0)

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

#### Environment variables declaration

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
