No warning is printed by default:

```ocaml
let () =
  let f ~x:() = () in
  f ();;
let x = 4
```

Warning attributes must be set to print them:

```ocaml version<4.12
[@@@warning "+6"]
let () =
  let f ~x:() = () in
  f ();;
let x = 4
```
```mdx-error
...
Warning 6: label x was omitted in the application of this function.
```

```ocaml version>=4.12
[@@@warning "+6"]
let () =
  let f ~x:() = () in
  f ();;
let x = 4
```
```mdx-error
Line 4, characters 5-6:
Warning 6 [labels-omitted]: label x was omitted in the application of this function.
```
