OCAMLOPT=ocamlopt

FLAGS=-I ../extlib -I ../swflib
FILES=tTFData tTFParser tTFTools tTFSwfWriter tTFCanvasWriter tTFJsonWriter
LIBS=extLib swflib unix

OUTPUT=ttf

all:
	$(OCAMLOPT) $(FLAGS) $(FILES:=.ml) -g -a -o ttf.cmxa

exec:
	$(OCAMLOPT) $(FLAGS) $(LIBS:=.cmxa) $(FILES:=.ml) main.ml -g -o $(OUTPUT)

clean:
	rm -rf ttf.cmxa ttf.lib ttf.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)
