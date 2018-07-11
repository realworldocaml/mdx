## mdx -- executable code blocks inside markdown files

`mdx` allows to execute code blocks inside markdow files.
There are (currently) two sub-commands, corresponding
to two modes of operations: pre-processing (`mdx pp`)
and tests (`mdx test`).

The pre-processor mode allows to mix documentation and code,
and to practice "literate programming" using markdown and OCaml.

The test mode allows to ensure that shell scripts and OCaml fragments
in the documentation always stays up-to-date.

`mdx` is released as a single binary (called `mdx`) and
can be installed using opam:

```sh
$ opam install mdx
```

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
- Lines beginning without a dollar sign are considered command *outputs*.
- Command outputs can contains *ellipsis*: `...`. These will
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

`mdx` will also consider exit codes when the syntax `[<exit code>]`is used:

    ```sh
    $ exit 1
    [1]
    ```

Note that nothing will be displayed when the exit code is 0 (e.g. in case
of success).

#### OCaml Code

`mdx` interprets OCaml fragments. It understands _normal_ code fragments and
_toplevel_ code fragments (starting with a `#` sign and optionally ending by
`;;`). Arbitrary whitespace padding is supported, at long as it stays
consistent within a code block.

Toplevel fragments interleaves OCaml code and their corresponding outputs.

Here is an example of normal OCaml code:

    ```ocaml
    print_endline "42"
    ```

Here is an examples of toplevel OCaml code:

    ```ocaml
    # print_endline "42"
    42
    ```

### Pre-processing

`mdx pp` allows to transform a markdow file into a valid
OCaml file, which can be passed to OCaml using the `-pp`
option.

For instance, given the following `file.md` document:

    ```ocaml
    # print_endline "42"
    42
    ```

Can be compiled and executed using:

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

Cram tests can be executed and checked with `mdx test <file.md>`.

    ```sh
     $ for i in `seq 1 10`; do echo $i; done
     1
     ...
     10
     ```

If the output is not consistent with what is expected,
`<file.md>.corrected` is generated.

#### OCaml

To execute OCaml code and toplevel fragments, uses `mdx test <file.md>`.

    ```ocaml
    # print_endline "42"
    42
    ```

If the output is not consistent with what is expected
`<file.md>.corrected` is generated.

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

This allows to test the consistency of a markdown file using the normal dev
workflow:

```
$ jbuilder runtest
```

will display a diff of the output if something has changed. For instance:

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

And the changes can then be accepted using:

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
described in the file. Use `mdx test --non-deterministic <file>` to come
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
