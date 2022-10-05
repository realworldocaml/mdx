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

The formatting for multilines in .mli files should be preserved exactly:

  {[
    match None with
    | None -> ()
    | Some a -> match a with
      | None -> ()
      | Some b -> b
  ]}

It should also work fine for toplevel descriptions (as in
[multilines/test-case.md]):

{@ocaml[
  # let rec fact = function
    | 1 -> 1
    | n -> n * fact (n-1)
    ;;
  val fact : int -> int = <fun>
]}
*)
