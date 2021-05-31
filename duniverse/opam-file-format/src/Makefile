all: opam-file-format.cmxa opam-file-format.cma

%.ml: %.mll
	ocamllex $<

OCAML_VERSION:=$(shell ocamlc -vnum | tr -d '\r' | sed -e 's/+.*//' -e 's/\.//g')
ifeq ($(OCAML_VERSION),)
OCAML_VERSION:=0
endif

ifeq ($(shell test $(OCAML_VERSION) -ge 4022 || echo no-attributes),)
PP=
else
PP=-pp 'sed -e s/\\[@@\\?ocaml[^]]*\\]//'
endif

show:
	echo $(OCAML_VERSION)

opamParserTypes.cmi: opamParserTypes.cmo

%.ml %.mli: %.mly
	ocamlyacc $<

%.cmi: %.mli
	ocamlc -c $(PP) $<

%.cmo: %.ml
	ocamlc -c $(PP) $<

%.cmx: %.ml
	ocamlopt -c $(PP) $<

MODULES = opamParserTypes opamBaseParser opamLexer opamParser opamPrinter

GENERATED = $(patsubst %.mly,%.ml,$(wildcard *.mly)) \
            $(patsubst %.mly,%.mli,$(wildcard *.mly)) \
            $(patsubst %.mll,%.ml,$(wildcard *.mll)) \
	    META

opam-file-format.cma: $(addsuffix .cmo,$(MODULES))
	ocamlc -a $^ -o $@

opam-file-format.cmxa: $(addsuffix .cmx,$(MODULES))
	ocamlopt -a $^ -o $@

opam-file-format.cmxs: $(addsuffix .cmx,$(MODULES))
	ocamlopt -shared $^ -o $@

META: META.in ../opam-file-format.opam $(wildcard *.cm*)
	sed -e 's/VERSION/$(shell sed -ne "s/^version: //p" ../opam-file-format.opam)/' \
	    $(if $(wildcard *.cmx*),,-e '/archive(native)/d') \
	    $(if $(wildcard *.cma),,-e '/archive(byte)/d') $< > $@

.PHONY: clean
clean:
	rm -f *.cm* *.o *.a $(GENERATED) .depend .merlin

.depend: *.ml *.mli $(GENERATED)
	ocamldep $(PP) *.mli *.ml > .depend

-include .depend
