#!/bin/bash
set -e

(
  cd basic
  duniverse opam
  duniverse lock
  duniverse pull
  dune build --profile release --root=.
  [[ -e _build/default/basic.exe ]]
)
