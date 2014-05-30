OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC=nast.ml nxml.ml binast.ml nbytecode.ml ncompile.ml

all: bytecode native

native: neko.cmxa

bytecode: neko.cma

neko.cmxa: $(SRC)
	$(OCAMLOPT) -I ../extlib -a -o neko.cmxa $(SRC)

neko.cma: $(SRC)
	$(OCAMLC) -I ../extlib -a -o neko.cma $(SRC)

clean:
	rm -rf neko.cmxa neko.cma neko.lib neko.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)

.PHONY: all bytecode native clean

Makefile: ;
$(SRC): ;
