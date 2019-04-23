#!/usr/bin/env bash
DISTRO=${DISTRO:-alpine}

set -ex
# TODO opam2 depext
case $DISTRO in
alpine*) sudo apk add m4 ;;
debian*) sudo apt update; sudo apt -y install m4 pkg-config ;;
ubuntu*) sudo apt update; sudo apt -y install m4 pkg-config ;;
esac

sudo chown -R opam /home/opam/src
cd /home/opam/src
echo "Installing dependencies"
opam pin add --no-action duniverse .
opam update
opam install --deps-only -t duniverse
opam install ocamlformat=0.9
echo "Building and running tests"
make
make test
make install
(cd examples
 ./build-examples.sh)
echo "Checking code formatting"
make format
if ! git diff --exit-code ; then
  echo "Incorrect formatting, please run 'make format' before comitting changes"
  exit 1
fi
