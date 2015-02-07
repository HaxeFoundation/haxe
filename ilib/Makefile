OCAMLOPT=ocamlopt
OCAMLC=ocamlc

SRCS=peData.ml peReader.ml peWriter.ml ilMeta.mli ilData.mli ilMetaTools.ml ilMetaDebug.ml ilMetaReader.ml

all: native bytecode

native: il.cmxa
bytecode: il.cma

il.cmxa: $(SRCS)
	$(OCAMLOPT) -g -I ../extlib -a -o il.cmxa $(SRCS)

il.cma: $(SRCS)
	$(OCAMLC) -g -I ../extlib -a -o il.cma $(SRCS)

dump: il.cmxa dump.ml peDataDebug.ml ilMetaDebug.ml
	$(OCAMLOPT) -g -I ../extlib -o dump ../extlib/extLib.cmxa il.cmxa peDataDebug.ml dump.ml

clean:
	rm -f il.cma il.cmxa il.lib il.a $(wildcard *.cmx) $(wildcard *.cmo) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) dump

.PHONY: all bytecode native clean

Makefile: ;
$(SRCS): ;
