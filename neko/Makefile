all:
	ocamlopt -I ../extlib -a -o neko.cmxa nast.ml nxml.ml binast.ml nbytecode.ml ncompile.ml

clean:
	rm -rf neko.cmxa neko.lib neko.a $(wilcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)