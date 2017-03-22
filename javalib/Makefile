OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC=jData.ml jReader.ml jWriter.ml

all: bytecode native

native: javalib.cmxa
bytecode: javalib.cma

javalib.cmxa: $(SRC)
	$(OCAMLOPT) -g -I ../extlib -a -o javalib.cmxa $(SRC)

javalib.cma: $(SRC)
	$(OCAMLC) -g -I ../extlib -a -o javalib.cma $(SRC)

clean:
	rm -rf javalib.cmxa javalib.cma javalib.lib javalib.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)

.PHONY: all bytecode native clean

Makefile: ;
$(SRC): ;
