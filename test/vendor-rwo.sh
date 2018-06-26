#!/usr/bin/env bash -ex

PACKAGES="core_extended async atdgen toplevel_expect_test patdiff sexp_pretty re fmt ppxlib lambdasoup cmdliner ppx_tools cohttp-async core_bench mtime yojson astring atd uri"
dune build
rm -rf _rwo
git clone git://github.com/realworldocaml/book _rwo
dune exec -- duniverse opam-lock $PACKAGES -vv
dune exec -- duniverse lock -vv
dune exec -- duniverse pull -r _rwo -vv
#jbuilder build bin/main.exe
#jbuilder clean
opam install -y --switch=4.06.1 dune num result menhir
opam exec --switch=4.06.1 -- dune build bin/main.exe
