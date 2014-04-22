OCAMLOPT=ocamlopt
OCAMLC=ocamlc

all: bytecode native

native:
	$(OCAMLOPT) -I ../extlib -a -o neko.cmxa nast.ml nxml.ml binast.ml nbytecode.ml ncompile.ml

bytecode:
	$(OCAMLC) -I ../extlib -a -o neko.cma nast.ml nxml.ml binast.ml nbytecode.ml ncompile.ml

clean:
	rm -rf neko.cmxa neko.cma neko.lib neko.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)
