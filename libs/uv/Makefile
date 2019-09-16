ALL_CFLAGS = $(CFLAGS)
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC = uv.ml uv_stubs.c

all: bytecode native

bytecode: uv.cma

native: uv.cmxa

uv.cma: uv_stubs.o uv.ml
	ocamlfind $(OCAMLC) -safe-string -a -o uv.cma uv.ml

uv.cmxa: uv_stubs.o uv.ml
	ocamlfind $(OCAMLOPT) -safe-string -a -o uv.cmxa uv.ml

uv_stubs.o: uv_stubs.c
	ocamlfind $(OCAMLC) -safe-string $(ALL_CFLAGS) uv_stubs.c

clean:
	rm -f $(wildcard *.cmo) $(wildcard *.cma)  $(wildcard *.cmx) $(wildcard *.cmxa) $(wildcard *.a) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)

.PHONY: all bytecode native clean
Makefile: ;
$(SRC): ;
