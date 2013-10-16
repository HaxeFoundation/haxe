OCAMLOPT=ocamlopt

all: dump
	$(OCAMLOPT) -g -I ../extlib -a -o il.cmxa iData.ml iReader.ml iWriter.ml

dump: dump.ml iData.ml iReader.ml iWriter.ml
	$(OCAMLOPT) -g -I ../extlib -o dump ../extlib/extLib.cmxa iData.ml iReader.ml iWriter.ml dump.ml 
	
clean:
	rm -f il.cmxa il.lib il.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)
