Toplevel blocks are formatted when setting option `--format-code`:

```ocaml
# module X = struct let x = 3 end
# let x = 2 in x + 1
# let f = function
  | (_, (_, _)) when (true || false) ->
  assert true;
  ignore @@ (fun x -> (ignore @@ (fun _ -> [2; 3; 3; 4; 5])))
  | _ -> ()
# let ( || ) x y = match x with true -> y | false -> false
```

Invalid blocks are not formatted:

```ocaml
# let x = 2 in
```

Regular OCaml blocks are not formatted:

```ocaml
let _ = let x = 2 in let y = 3 in x + y

module X = struct let x = 3 end
```
