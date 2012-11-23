all:
	ocamlopt -g -I ../extlib -a -o java.cmxa jData.mli jReader.ml
	
clean:
	rm -rf java.cmxa java.lib java.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)