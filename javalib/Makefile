OCAMLOPT=ocamlopt

all:
	$(OCAMLOPT) -g -I ../extlib -a -o java.cmxa jData.ml jReader.ml
	
clean:
	rm -rf java.cmxa java.lib java.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)
