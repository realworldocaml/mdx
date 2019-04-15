.PHONY: build clean test

build:
	dune build duniverse.install README.md --profile=release

clean:
	dune clean

doc:
	dune build @doc --profile=release

test:
	dune runtest

help-%:
	dune exec --profile=release -- duniverse $(*F) --help

git-lock:
	dune exec --profile=release -- duniverse git-lock $(DEBUG)

git-pull:
	dune exec --profile=release -- duniverse git-pull $(DEBUG)

git-merge:
	dune exec --profile=release -- duniverse git-merge $(DEBUG)

publish-doc: doc
	rm -rf .gh-pages
	git clone `git config --get remote.origin.url` .gh-pages --reference .
	git -C .gh-pages checkout --orphan gh-pages
	git -C .gh-pages reset
	git -C .gh-pages clean -dxf
	cp -r _build/default/_doc/_html/* .gh-pages/
	git -C .gh-pages add .
	git -C .gh-pages commit -m "Update Pages"
	git -C .gh-pages push origin gh-pages -f
	rm -rf .gh-pages

test-all:
	sh ./.docker-run.sh

REPO=../../mirage/opam-repository
PACKAGES=$(REPO)/packages
# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)

