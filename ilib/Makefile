OCAMLOPT=ocamlopt

all: il.cmxa dump

SRCS=peData.ml peReader.ml peWriter.ml ilMeta.mli ilMetaReader.ml

il.cmxa:$(SRCS)
	$(OCAMLOPT) -g -I ../extlib -a -o il.cmxa $(SRCS)

dump: il.cmxa dump.ml peDataDebug.ml
	$(OCAMLOPT) -g -I ../extlib -o dump ../extlib/extLib.cmxa il.cmxa peDataDebug.ml dump.ml
	
clean:
	rm -f il.cmxa il.lib il.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) dump

.PHONY: all 
