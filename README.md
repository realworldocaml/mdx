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
- Lines beginning without a dollar sign are considered command *output*.
- Command outputs can contains *ellipsis*: `...`. These will
  match any possible outputs (on zero, one or multiple lines).

Arbitrary padding with whitespace is supported, as long as it is consistent
inside a code block.

    Here is an example of a markdown file using shell scripts inside code blocks:

    ```sh
    $ for i in `seq 1 10`
    1
    ...
    3
    ```

`mdx` will also consider exit codes when the following syntax is used:

    ```sh
    $ <command>
    <output>
    ✘ exit 1
    ```

Note that nothing is displayed when the exit code is 0 (success).

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

This can be automated using `dune`:

```
(rule
 ((targets (file.ml))
  (deps    (file.md))
  (action  (with-stdout-to ${@} (run mdx pp ${<})))))

(executable ((name file)))
```

### Tests

#### Cram Tests

Cram tests can be executed and checked with `mdx test`:

    Here a cram test:

    ```sh
     $ for i in `seq 1 10`; do echo $i; done
     1
     ...
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

#### Integration with Dune

To test that the code blocks of `file.md` stays consistent, one can use
dune's `diff?` stanza:

```
(alias
 ((name runtest)
  (deps (file.md))
  (action (progn
           (run mdx test ${<})
           (diff? ${<} ${<}.corrected)))))
```

This allows to test `file.md` by doing:

```
$ jbuilder runtest
```

This will display a diff of the output if something has changed. For instance:

```
$ jbuilder runtest
------ file.md
++++++ file.md.corrected
File "file.md", line 23, characters 0-1:
 |
 |```sh
-| $ for i in `seq 1 3`; do echo $i; done
+| $ for i in `seq 1 4`; do echo $i; done
 | 1
 | 2
 | 3
+| 4
 |```
```

The changes can then be accepted using:

```
$ jbuilder promote
```

#### Non-deterministic Tests

**Non-deterministic Outputs**

`mdx test` supports non-deterministic outputs:

    ```sh non-deterministic=output
    $ <command>
    <output>
    ```

In that case, `ppx test <file>` will run the command but will not
generate `<file>.corrected` if the new output differs from the one
described in the file. Use `ppx test --non-deterministic <file>` to come
back to the default behaviour.

**Non-deterministic Commands**

`mdx test` supports non-deterministic commands:

    ```ocaml non-deterministic=command
    # Random.int 10;;
    - : int = 5
    ```

In that case, `mdx test <file>` will *not* run the command. Use `mdx test
--non-deterministic <file>` to come back to the default behaviour.

### Sections

It is possible to test or execute only a subset of the file using
sections using the `--section` option (short name is `-s`). For
instance `mdx pp -s foo` will only consider the section matching the
perl regular expression `foo`.
