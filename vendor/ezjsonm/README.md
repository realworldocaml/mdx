## Ezjsonm

An easy interface on top of the
[Jsonm](http://erratique.ch/software/jsonm) library.

This version provides more convenient (but far less flexible)
input and output functions that go to and from [string] values.
This avoids the need to write signal code, which is useful for
quick scripts that manipulate JSON.

More advanced users should go straight to the Jsonm[1] library and
use it directly, rather than be saddled with the Ezjsonm interface.

### Examples

For instance, you can explore the HTTP status code specified in the
JSON format [here](https://github.com/citricsquid/httpstatus.es).
After downloading [that
file](https://raw.github.com/citricsquid/httpstatus.es/master/codes/4.json),
you can open an OCaml toplevel and write:

```ocaml
# #require "ezjsonm";;
# let json = Ezjsonm.from_channel (open_in "4.json");;
# Ezjsonm.(get_string (find json ["codes"; "418"; "summary"]))
- : string = "I'm a teapot"
```
