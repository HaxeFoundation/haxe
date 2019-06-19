ALL_CFLAGS = $(CFLAGS) -I .
LIBS =
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC=objsize.mli objsize.ml

all: bytecode native

bytecode: objsize.cma

native: objsize.cmxa

objsize.cma: c_objsize.o $(SRC)
	$(OCAMLC) -safe-string -a -o objsize.cma $(LIBS) $(SRC)

objsize.cmxa: c_objsize.o $(SRC)
	$(OCAMLOPT) -safe-string -a -o objsize.cmxa $(LIBS) $(SRC)

c_objsize.o: c_objsize.c
	$(OCAMLC) -safe-string $(ALL_CFLAGS) c_objsize.c

clean:
	rm -rf $(wildcard *.cma) $(wildcard *.cmxa) $(wildcard *.cmx) $(wildcard *.cmi) $(wildcard *.cmo) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.a)

.PHONY: all bytecode native clean

Makefile: ;
$(SRC): ;
c_objsize.c: ;
