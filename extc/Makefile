ALL_CFLAGS = $(CFLAGS) -I zlib
LIBS = -I ../extlib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC = extc.ml process.ml extc_stubs.c process_stubs.c

all: bytecode native

bytecode: extc.cma

native: extc.cmxa

extc.cma: extc_stubs.o process_stubs.o extc.ml process.ml
	$(OCAMLC) -a -o extc.cma $(LIBS) extc.ml process.ml

extc.cmxa: extc.ml process.ml extc_stubs.o process_stubs.o
	$(OCAMLOPT) -a -o extc.cmxa $(LIBS) extc.ml process.ml

extc_stubs.o: extc_stubs.c
	$(OCAMLC) $(ALL_CFLAGS) extc_stubs.c

process_stubs.o: process_stubs.c
	$(OCAMLC) $(ALL_CFLAGS) process_stubs.c
	
clean:
	rm -f extc.cma extc.cmi extc.cmx extc.cmxa extc.o extc.obj extc.lib extc_stubs.obj extc_stubs.o process.cmx process.obj process.cmi process.o process_stubs.obj process_stubs.o
	rm -f extc.a libextc.a libextc.lib extc.cmo process.cmo

test: native
	ocamlopt -o test.exe -I ../extLib extLib.cmxa zlib/zlib.lib extc_stubs.obj process_stubs.obj extc.cmxa test.ml
	
.PHONY: all bytecode native clean
Makefile: ;
$(SRC): ;
