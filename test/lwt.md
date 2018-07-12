#require directives work as expected:

```ocaml
# #require "lwt"
# let x = Lwt.return 3
val x : int Lwt.t = <abstr>
```
