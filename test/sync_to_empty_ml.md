This test is a reggresion test for [PR 137](https://github.com/realworldocaml/mdx/pull/137).
It verifies that the promotion to a ml file is done even if it is empty.

```ocaml file=sync_to_empty_ml.ml
let f = "hello world!"
```