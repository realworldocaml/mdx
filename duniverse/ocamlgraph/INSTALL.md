
The simplest way to install OCamlGraph is via opam.

If instead you choose to compile OCamlGraph from sources,
the simplest way is to use `dune`.

Otherwise, you can still use the old way:
- Configure with
  ```
    autoconf
	./configure
  ```
- Compile with
  ```
	make
  ```
- Install (as root if needed) with
  ```
	make install
  ```
  To install somewhere else, with a different path prefix, use instead
  ```
        make DESTDIR=/another/place install
  ```
  findlib users may also do
  ```
	make install-findlib
  ```
