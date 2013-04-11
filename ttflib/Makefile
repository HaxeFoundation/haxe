all:
	ocamlopt -I ../extlib -I ../swflib tTFData.ml tTFParser.ml tTFTools.ml tTFSwfWriter.ml tTFCanvasWriter.ml tTFJsonWriter.ml main.ml -g -a -o ttf.cmxa

clean:
	rm -rf ttf.cmxa ttf.lib ttf.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)