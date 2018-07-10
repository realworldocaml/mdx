## mdx -- executable code blocks inside markdown files

`mdx` allows to execute code blocks inside markdow files.
There are (currently) two sub-commands, corresponding
to two modes of operations: pre-processing (`mdx pp`)
and tests (`mdx test`).

The pre-processor mode allows to mix documentation and code,
and to practice "literate programming" using markdown and OCaml.

The test mode allows to ensure that shell scripts and OCaml fragments
in the documentation always stays up-to-date.

### Supported Extensions

#### Shell Scripts

`mdx` interprets shell scripts inside `sh` code blocks as cram-like tests. The
syntax is the following:

- Lines beginning with a dollar sign and a space are
  *commands* and will be run in the shell.
- Multi-lines commands end by `\` and continue with two spaces and
  a `>` sign on the next line:
  ```
  $ <line1> \
  > <line2> \
  > <line3>
  ```
- Commands support the heredoc syntax (`<<`):
  ```
  $ cat <<EOF \
  > hello\
  > world\
  > EOF
  hello
  world
  ```
- Lines beginning without a dollar sign are considered command *output*.
- Command outputs can contains *ellipsis*: `...`. These will
  match any possible outputs (on zero, one or multiple lines).

Arbitrary padding with whitespace is supported, as long as it is consistent
inside a code block.

   Here is an example of a markdown file using shell scripts inside code blocks:

   ```sh
   $ for i in `seq 1 3`
   1
   2
   3
   ```

#### OCaml Code

`mdx` interprets OCaml fragments. It understands "normal" code fragments and
"toplevel" code fragments (starting with a `#` sign). Arbitrary (whitespace)
padding is supported, at long as it stays consistent.

Toplevel fragments interleaves OCaml code and their corresponding outputs.

    Here is an example of *normal* OCaml code:

    ```ocaml
    print_endline "42"
    ```

    Here is an examples of *toplevel* OCaml code:

    ```ocaml
    # print_endline "42"
    42
    ```

### Pre-processing

`mdx pp` allows to transform a markdow file into a valid
OCaml file, which can be passed to OCaml using the `-pp`
option.

For instance, given the following `file.md` document:

    Print 42 on the standard output:

    ```ocaml
    # print_endline "42"
    42
    ```

This can be compiled and executed using:

```sh
$ ocamlc -pp 'mdx pp' -impl file.md -o file.exe
$ ./file.exe
42
```

TODO: dune integration

### Tests

#### Cram Tests

Cram tests can be executed and checked with `mdx test`:

    Here a cram test:

    ```sh
     $ for i in `seq 1 3`; do echo $i; done
     1
     2
     3
     ```

That file can be executed with `mdx test <file.md>`. If the output
differs, a `<file.md>.corrected` file is generated, which means
that integration with dune can use it (using the `diff?` stanza)
to outline changes and to automatically promote thechange with
`dune promote`.

#### OCaml

To test OCaml code and toplevel fragments, uses `mdx test <file.md>`.
For instance:

```ocaml
# print_endline "42";;
42
```

If the output is not consistent with what is expected: `mdx test
<file.md>` will produce a `<file.md>.corrected`. This can be integrated
with the `diff?` stanza of dune so that `dune promote` will automatically
update the file if the output differs.

TODO: test compilation

### Sections

It is possible to test or execute only a subset of the file using sections.
`mdx pp -s foo` will only consider the section matching the perl regular
expression `foo`.
