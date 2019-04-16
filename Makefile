.PHONY: build clean test doc install

build:
	dune build 

clean:
	dune clean

doc:
	dune build @doc

test:
	dune runtest

install:
	dune install
