#!/usr/bin/env bash -ex

jbuilder build
rm -rf _test
git clone --depth=1 git://github.com/mirage/mirage _test
cd _test
../_build/install/default/bin/duniverse dune-fetch -vv  -f ../duniverse-dune.lock
#jbuilder build bin/main.exe
#jbuilder clean
opam --switch=4.06.1 install -y jbuilder num result
opam --switch=4.06.1 exec -- jbuilder build bin/main.exe
