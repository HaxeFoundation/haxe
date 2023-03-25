ALL_CFLAGS = $(CFLAGS) -I pcre
LIBS =
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC = pcre.ml pcre_stubs.c

all: bytecode native

bytecode: pcre.cma

native: pcre.cmxa

pcre.cma: pcre_stubs.o pcre.ml
	$(OCAMLC) -safe-string -a -o pcre.cma $(LIBS) pcre.ml

pcre.cmxa: pcre.ml pcre_stubs.o
	$(OCAMLOPT) -safe-string -a -o pcre.cmxa $(LIBS) pcre.ml

pcre_stubs.o: pcre_stubs.c
	$(OCAMLC) -safe-string $(ALL_CFLAGS) pcre_stubs.c

clean:
	rm -f pcre.cma pcre.cmi pcre.cmx pcre.cmxa pcre.o pcre.obj pcre_stubs.obj pcre_stubs.o
	rm -f pcre.a libpcre2-8.a libpcre2-8.lib pcre.cmo

.PHONY: all bytecode native clean
Makefile: ;
$(SRC): ;
