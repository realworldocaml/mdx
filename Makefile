.PHONY: build clean test doc install format

PREFIX ?= /usr/local/bin

build:
	dune build 

clean:
	dune clean

doc:
	dune build @doc

test:
	dune runtest


install:
	mkdir -p $(PREFIX)
	cp _build/install/default/bin/duniverse $(PREFIX)/bin

update: build
	dune exec -- duniverse init duniverse alcotest 
	rm -rf duniverse
	dune exec -- duniverse pull

format:
	- dune build @fmt 2> /dev/null
	dune promote
