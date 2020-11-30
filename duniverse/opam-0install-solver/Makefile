.PHONY: all build-tests test

all:
	dune build

build-tests:
	dune build ./test/test.exe ./test/dump.exe

test: build-tests
	dune exec ./test/test.exe
