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
	rm -rf *.cma *.cmxa *.cmi *.obj *.o *.cmx