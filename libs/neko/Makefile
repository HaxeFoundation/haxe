all:
	ocamlopt -I ../extlib -a -o neko.cmxa nast.ml binast.ml nxml.ml
	
clean:
	rm -rf neko.cmxa neko.lib neko.a  $(wilcard *.cmx)  $(wilcard *.obj)  $(wilcard *.o)  $(wilcard *.cmi)