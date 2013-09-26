CFLAGS = -I zlib
LIBS = -I ../extlib
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

all: bytecode native

bytecode: extc_stubs.obj
	$(OCAMLC) -a -o extc.cma $(LIBS) extc.ml

native: extc_stubs.obj
	$(OCAMLOPT) -a -o extc.cmxa $(LIBS) extc.ml

extc_stubs.obj: extc_stubs.c
	$(OCAMLC) $(CFLAGS) extc_stubs.c

clean:
	rm -f extc.cma extc.cmi extc.cmx extc.cmxa extc.o extc.obj extc.lib extc_stubs.obj extc_stubs.o
	rm -f extc.a libextc.a libextc.lib extc.cmo
