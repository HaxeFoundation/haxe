OCAMLOPT=ocamlopt
OCAMLC=ocamlc

SRCS=peData.ml peReader.ml peWriter.ml ilMeta.mli ilData.mli ilMetaTools.ml ilMetaDebug.ml ilMetaReader.ml

all: native bytecode

native: ilib.cmxa
bytecode: ilib.cma

ilib.cmxa: $(SRCS)
	ocamlfind $(OCAMLOPT) -g -package extlib -safe-string -a -o ilib.cmxa $(SRCS)

ilib.cma: $(SRCS)
	ocamlfind $(OCAMLC) -g -package extlib -safe-string -a -o ilib.cma $(SRCS)

dump: ilib.cmxa dump.ml peDataDebug.ml ilMetaDebug.ml
	ocamlfind $(OCAMLOPT) -g -package extlib -safe-string -o dump ../extlib/extLib.cmxa ilib.cmxa peDataDebug.ml dump.ml

clean:
	rm -f ilib.cma ilib.cmxa ilib.lib ilib.a $(wildcard *.cmx) $(wildcard *.cmo) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) dump

.PHONY: all bytecode native clean

Makefile: ;
$(SRCS): ;
