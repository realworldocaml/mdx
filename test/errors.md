Errors should be well localized:

```ocaml
# class ['a] stack init = object
    val mutable v = init

    method pop =
      match v with
      | hd :: tl ->
        v <- tl;
        Some hd
      | [] -> None

    method push hd =
      v <- hd :: v
  end;;
Characters 0-191:
Error: Some type variables are unbound in this type:
         class ['a] stack :
           'b list ->
           object
             val mutable v : 'b list
             method pop : 'b option
             method push : 'b -> unit
           end
       The method pop has type 'b option where 'b is unbound
```

Hi!
