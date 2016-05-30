OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC=jData.ml jReader.ml jWriter.ml

all: bytecode native

native: java.cmxa
bytecode: java.cma

java.cmxa: $(SRC)
	$(OCAMLOPT) -g -I ../extlib -a -o java.cmxa $(SRC)

java.cma: $(SRC)
	$(OCAMLC) -g -I ../extlib -a -o java.cma $(SRC)

clean:
	rm -rf java.cmxa java.cma java.lib java.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)

.PHONY: all bytecode native clean

Makefile: ;
$(SRC): ;
