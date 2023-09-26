#!/bin/bash

version=ebfd3b9489e44187da2c67d79a32b6fc1e92bda4

set -e -o pipefail

TMP="$(mktemp -d)"
trap "rm -rf $TMP" EXIT

rm -rf odoc-parser
mkdir -p odoc-parser/src

(
    cd $TMP
    git clone https://github.com/ocaml-doc/odoc-parser.git
    cd odoc-parser
    git checkout $version
)

SRC=$TMP/odoc-parser

cp -v $SRC/src/*.{ml,mli,mll} odoc-parser/src
cp -v $SRC/LICENSE.md odoc-parser/

git checkout odoc-parser/src/dune
git add -A .
