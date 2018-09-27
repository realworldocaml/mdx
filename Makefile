.PHONY: all test

all:
	dune build

install:
	dune install

test:
	dune runtest

clean:
	dune clean
