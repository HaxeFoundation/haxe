OCAMLOPT=ocamlopt

all: il.cmxa

SRCS=peData.ml peReader.ml peWriter.ml ilMeta.mli ilData.mli ilMetaTools.ml ilMetaDebug.ml ilMetaReader.ml 

il.cmxa:$(SRCS)
	$(OCAMLOPT) -g -I ../extlib -a -o il.cmxa $(SRCS)

dump: il.cmxa dump.ml peDataDebug.ml ilMetaDebug.ml
	$(OCAMLOPT) -g -I ../extlib -o dump ../extlib/extLib.cmxa il.cmxa peDataDebug.ml dump.ml
	
clean:
	rm -f il.cmxa il.lib il.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) dump

.PHONY: all 
