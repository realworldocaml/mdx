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

v-setup:
	jbuilder exec -- duniverse opam duniverse
	jbuilder exec -- duniverse lock

v:
	git checkout vendor 2>/dev/null || git checkout -b vendor
	git merge master
	jbuilder exec -- duniverse pull
	rm -rf vendor/opam-core/src_ext
	git commit -m 'trim opam/src-ext' -a || true
	git checkout master
	git merge vendor --squash
	git commit -m 'update vendor libraries' -a || true
	cd vendor/opam-core && ./configure && cp src/core/opamVersion.ml.in src/core/opamVersion.ml

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

