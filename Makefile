.PHONY: build clean test doc

build:
	dune build 

clean:
	dune clean

doc:
	dune build @doc

test:
	dune runtest
