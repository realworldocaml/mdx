# OCaml JIT compiler

To give it a quick spin:
```
git clone git@github.com:NathanReb/ocaml-jit.git
cd ocaml-jit
opam compiler create --switch=. NathanReb:jit-hook-411
opam pin compiler-libs-opttoplevel ./
opam install --deps-only ./
dune exec jittop
```
