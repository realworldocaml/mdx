## mdx -- executable code blocks inside markdown files

`mdx` allows to execute code blocks inside markdow files.

### Supported extensions

#### Cram tests

Codes blocks using `sh` are considered as cram tests:

```sh
  $ for i in `seq 1 3`; do echo $i; done
  1
  2
  3
```

which can be executed with `mdx test <file.md>`. If the output
differs, a `<file.md>.corrected` file is generated, which means
that integration with dune can use it (using the `diff?` stanza)
to outline changes and to automatically promote thechange with
`jbuilder promote`.

#### OCaml code

Code blocks using `ocaml` are considered either normal OCaml files
or toplevel fragements if the first line starts by `#`.

To execute OCaml code (or toplevel fragments), uses `mdx pp <file.md>`.
For instance:

```ocaml
# print_endline "42";;
42
```

can be compiled and executed with
`ocamlc -pp 'mdx pp' -impl <file.md> && ./a.out`

The same file can be tested to check that the output is consistent with
what is expected: `mdx tex <file.md>` will produce a `<file.md>.corrected`
file if the output needs to be corrected.


### Sections

It is possible to test or execute only a subset of the file using sections.
`mdx pp -s foo` will only consider the section matching the perl regular
expression `foo`.
