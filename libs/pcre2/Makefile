ALL_CFLAGS = $(CFLAGS) -I pcre2
LIBS =
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC = pcre2.ml pcre2_stubs.c

all: bytecode native

bytecode: pcre2.cma

native: pcre2.cmxa

pcre2.cma: pcre2_stubs.o pcre2.ml
	$(OCAMLC) -safe-string -a -o pcre2.cma $(LIBS) pcre2.ml

pcre2.cmxa: pcre2.ml pcre2_stubs.o
	$(OCAMLOPT) -safe-string -a -o pcre2.cmxa $(LIBS) pcre2.ml

pcre2_stubs.o: pcre2_stubs.c
	$(OCAMLC) -safe-string $(ALL_CFLAGS) pcre2_stubs.c

clean:
	rm -f pcre2.cma pcre2.cmi pcre2.cmx pcre2.cmxa pcre2.o pcre2.obj pcre2_stubs.obj pcre2_stubs.o
	rm -f pcre2.a libpcre2-8.a libpcre2-8.lib pcre2.cmo

.PHONY: all bytecode native clean
Makefile: ;
$(SRC): ;
