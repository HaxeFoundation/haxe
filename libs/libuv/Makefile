ALL_CFLAGS = $(CFLAGS)
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC = libuv.ml libuv_stubs.c test.ml

all: bytecode test #native

bytecode: libuv.cma

#native: libuv.cmxa

test: test.ml libuv.ml
	ocamlfind $(OCAMLC) -o test -safe-string -package extlib libuv_stubs.c libuv.ml test.ml -dllib dlllibuv_stubs.so

libuv.cma: libuv_stubs.so libuv.ml
	ocamlfind $(OCAMLC) -safe-string -a -o libuv.cma libuv.ml -dllib dlllibuv_stubs.so

#libuv.cmxa: libuv_stubs.so libuv.ml
#	ocamlfind $(OCAMLOPT) -safe-string -a -o libuv.cmxa libuv.ml -dllib dlllibuv_stubs.so

libuv_stubs.o: libuv_stubs.c
	ocamlfind $(OCAMLC) -o libuv_stubs -safe-string $(ALL_CFLAGS) libuv_stubs.c

libuv_stubs.so: libuv_stubs.o
	ocamlfind ocamlmklib -o libuv_stubs $(ALL_CFLAGS) libuv_stubs.o -luv

clean:
	rm -f libuv.cma libuv.cmi libuv.cmx libuv.cmxa libuv.o libuv.obj libuv.lib libuv_stubs.obj libuv_stubs.o
	rm -f libuv.a liblibuv.a liblibuv.lib libuv.cmo
	rm -f test.cmi test.cmx test.cmo test
	rm -f dlllibuv_stubs.so dlllibuv_stubs.a liblibuv_stubs.so liblibuv_stubs.a

.PHONY: all bytecode clean # native
Makefile: ;
$(SRC): ;
