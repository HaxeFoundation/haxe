ZLIB = zlib.lib
CFLAGS = -I zlib
LIBS = -cclib $(ZLIB) -cclib extc_stubs.obj

all: bytecode native

test: bytecode
	ocamlc -custom -o test.exe -cclib $(ZLIB) extLib.cma extc.cma test.ml

test-opt: native
	ocamlopt -o test.exe -cclib $(ZLIB) extLib.cmxa extc.cmxa test.ml

bytecode: extc_stubs.obj
	ocamlc -a -o extc.cma $(LIBS) extc.mli extc.ml

native: extc_stubs.obj
	ocamlopt -a -o extc.cmxa $(LIBS) extc.mli extc.ml

extc_stubs.obj: extc_stubs.c
	ocamlc $(CFLAGS) extc_stubs.c

clean:
	rm -f extc.cma extc.cmi test.cmi extc.cmx extc.cmxa extc.o extc.obj extc.lib extc_stubs.obj extc_stubs.o
	rm -f extc.a libextc.a libextc.lib extc.cmo test.cmo test.exe text-opt.exe test.cmx test.o test.obj
