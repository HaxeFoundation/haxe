all:
	ocamlopt -g -I ../extlib -I ../extc -a -o zip.cmxa zlib.mli zlib.ml zip.mli zip.ml
	
clean:
	rm -rf zip.cmxa zip.lib zip.a *.cmx *.obj *.o *.cmi