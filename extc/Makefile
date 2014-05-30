CFLAGS = -I zlib
LIBS = -I ../extlib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC = extc.ml extc_stubs.c

all: bytecode native

bytecode: extc.cma

native: extc.cmxa

extc.cma: extc_stubs.o extc.ml
	$(OCAMLC) -a -o extc.cma $(LIBS) extc.ml

extc.cmxa: extc.ml extc_stubs.o
	$(OCAMLOPT) -a -o extc.cmxa $(LIBS) extc.ml

extc_stubs.o: extc_stubs.c
	$(OCAMLC) $(CFLAGS) extc_stubs.c

clean:
	rm -f extc.cma extc.cmi extc.cmx extc.cmxa extc.o extc.obj extc.lib extc_stubs.obj extc_stubs.o
	rm -f extc.a libextc.a libextc.lib extc.cmo

.PHONY: all bytecode native clean
Makefile: ;
$(SRC): ;
