.PHONY: build test clean

build:
	dune build

test: build
	dune runtest

clean:
	dune clean
