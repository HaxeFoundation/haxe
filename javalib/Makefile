OCAMLOPT=ocamlopt
OCAMLC=ocamlc

all: bytecode native

native:
	$(OCAMLOPT) -g -I ../extlib -a -o java.cmxa jData.ml jReader.ml

bytecode:
	$(OCAMLC) -g -I ../extlib -a -o java.cma jData.ml jReader.ml

clean:
	rm -rf java.cmxa java.cma java.lib java.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)
