OCAMLOPT=ocamlopt

all:
	$(OCAMLOPT) -g -I ../extlib -a -o il.cmxa iData.ml iReader.ml iWriter.ml
	
clean:
	rm -f il.cmxa il.lib il.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)
