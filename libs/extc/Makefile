CFLAGS = -I zlib
LIBS = -I ../extlib

all: bytecode native

bytecode: extc_stubs.obj
	ocamlc -a -o extc.cma $(LIBS) extc.ml

native: extc_stubs.obj
	ocamlopt -a -o extc.cmxa $(LIBS) extc.ml

extc_stubs.obj: extc_stubs.c
	ocamlc $(CFLAGS) extc_stubs.c

clean:
	rm -f extc.cma extc.cmi extc.cmx extc.cmxa extc.o extc.obj extc.lib extc_stubs.obj extc_stubs.o
	rm -f extc.a libextc.a libextc.lib extc.cmo
