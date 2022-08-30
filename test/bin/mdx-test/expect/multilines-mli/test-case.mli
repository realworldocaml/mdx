(**

In OCaml docstring everything is indented with two spaces

Test multi-lines shell commands:

{@sh[
  $ for i in `seq 1 10`; do \
  >   echo $i; \
  > done
  1
  ...
  10
]}

This works for normal OCaml fragments:

{[
  let rec fact = function
  | 1 -> 1
  | n -> n * fact (n-1)
]}

The formatting for multilines in .mli files is the following:
- The first line is indented two spaces after the comment opening
- The other lines are indented to keep the original indentation relative to the
  first line

  {[
    match None with
    | None -> ()
    | Some a -> match a with
      | None -> ()
      | Some b -> b
  ]}

But it does not work fine for toplevel (see [multilines/test-case.md] for the
correct definition of [fact], which get erased by [mdx]):

{[
  # let rec fact = function;;
  Line 1, characters 24-26:
  Error: Syntax error
]}
*)
