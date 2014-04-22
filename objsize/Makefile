CFLAGS = -I .
LIBS =
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

all: bytecode native

bytecode: c_objsize.obj
	$(OCAMLC) -a -o objsize.cma $(LIBS) objsize.mli objsize.ml

native: c_objsize.obj
	$(OCAMLOPT) -a -o objsize.cmxa $(LIBS) objsize.mli objsize.ml

c_objsize.obj: c_objsize.c
	$(OCAMLC) $(CFLAGS) c_objsize.c

clean:
	rm -rf $(wildcard *.cma) $(wildcard *.cmxa) $(wildcard *.cmx) $(wildcard *.cmi) $(wildcard *.cmo) $(wildcard *.obj) $(wildcard *.o)