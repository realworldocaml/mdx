.PHONY: build clean test

build:
	jbuilder build duniverse.install

test:
	jbuilder runtest

install:
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean

doc:
	jbuilder build @doc

v-update:
	git checkout master
	jbuilder exec -- duniverse opam duniverse
	jbuilder exec -- duniverse lock
	git commit .duniverse -m 'update duniverse lockfiles' || true

v-pull:
	git checkout duniverse 2>/dev/null || git checkout -b duniverse
	git merge master --commit -m 'merge from master branch'
	jbuilder exec -- duniverse pull -v
	git push -u origin duniverse
	git checkout master

v-merge:
	git checkout master
	git merge duniverse --squash
	git commit -m 'update vendor libraries' -a || true

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

