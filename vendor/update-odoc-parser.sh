#!/bin/bash

version=5ac1ffc67ce1b96f5a990fa4902a157a5cdb42d0

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf odoc-parser
mkdir -p odoc-parser/src

(
    cd $TMP
    git clone https://github.com/ocaml/odoc.git
    cd odoc
    git -c advice.detachedHead=false checkout $version
)

SRC=$TMP/odoc

cp -v $SRC/src/parser/*.{ml,mli,mll} odoc-parser/src
cp -v $SRC/LICENSE odoc-parser/

git checkout odoc-parser/src/dune
git add -A .
