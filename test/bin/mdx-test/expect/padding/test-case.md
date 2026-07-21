Arbitrary padding is allowed, as long as it is consistent inside a code block.

```sh
   $ echo foo
   foo
```

```ocaml
     # let x = 3;;
     val x : int = 3
```

```sh version<4.12
$ ocaml -warn-help | grep -F ' 9 Missing'
  9 Missing fields in a record pattern.
```
```sh version>=4.12
$ ocaml -warn-help | grep -F ' 9 [missing-record-field-pattern] Missing'
  9 [missing-record-field-pattern] Missing fields in a record pattern.
```
