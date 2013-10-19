OCAMLOPT=ocamlopt

all: il.cmxa dump

il.cmxa: iData.ml iReader.ml iWriter.ml
	$(OCAMLOPT) -g -I ../extlib -a -o il.cmxa iData.ml iReader.ml iWriter.ml

dump: il.cmxa dump.ml iDataDebug.ml
	$(OCAMLOPT) -g -I ../extlib -o dump ../extlib/extLib.cmxa il.cmxa iDataDebug.ml dump.ml
	
clean:
	rm -f il.cmxa il.lib il.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) dump

.PHONY: all 
