#!/bin/bash
set -e

(
  cd basic
  duniverse init
  duniverse pull
  dune build --profile release --root=.
  [[ -e _build/default/basic.exe ]]
)
