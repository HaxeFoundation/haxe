OCAMLOPT=ocamlopt
OCAMLC=ocamlc

FLAGS=-I ../extlib -I ../swflib
FILES=tTFData tTFParser tTFTools tTFSwfWriter tTFCanvasWriter tTFJsonWriter
LIBS=extLib swflib unix

OUTPUT=ttf

all: native bytecode

native: ttf.cmxa

bytecode: ttf.cma

ttf.cmxa: $(FILES:=.ml)
	$(OCAMLOPT) $(FLAGS) $(FILES:=.ml) -g -a -o ttf.cmxa

ttf.cma: $(FILES:=.ml)
	$(OCAMLC) $(FLAGS) $(FILES:=.ml) -g -a -o ttf.cma

exec:
	$(OCAMLOPT) $(FLAGS) $(LIBS:=.cmxa) $(FILES:=.ml) main.ml -g -o $(OUTPUT)

clean:
	rm -rf ttf.cmxa ttf.cma ttf.lib ttf.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)

.PHONY: all native bytecode clean exec

Makefile: ;
$(FILES:=.ml): ;
