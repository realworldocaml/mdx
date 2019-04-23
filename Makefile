.PHONY: build clean test doc install format

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

format:
	- dune build @fmt 2> /dev/null
	dune promote
