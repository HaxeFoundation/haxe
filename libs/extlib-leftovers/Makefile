# Makefile contributed by Alain Frisch
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

MODULES = \
 multiArray rbuffer uCharExt uTF8

# the list is topologically sorted

MLI = $(MODULES:=.mli)
SRC = $(MLI) $(MODULES:=.ml)

all: bytecode native

opt: native

bytecode: extlib-leftovers.cma

native: extlib-leftovers.cmxa

extlib-leftovers.cma: $(SRC)
	$(OCAMLC) -safe-string -a -o extlib-leftovers.cma $(SRC)

extlib-leftovers.cmxa: $(SRC)
	$(OCAMLOPT) -safe-string -g -a -o extlib-leftovers.cmxa $(SRC)

clean:
	rm -f $(wildcard *.cmo) $(wildcard *.cmx) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cma) $(wildcard *.cmxa) $(wildcard *.a) $(wildcard *.lib) $(wildcard *.obj)
	rm -Rf doc

.PHONY: all opt bytecode native doc copy install uninstall clean

Makefile: ;

$(SRC): ;
