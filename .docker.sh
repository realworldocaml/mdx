#!/bin/bash
set -e

if [ -z "$OCAMLFORMAT" ]; then
  docker build --build-arg DISTRO=$DISTRO -f .dockerfiles/opam .
else
  docker build --build-arg OCAMLFORMAT=$OCAMLFORMAT -f .dockerfiles/ocamlformat .
fi
