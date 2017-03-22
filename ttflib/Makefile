OCAMLOPT=ocamlopt
OCAMLC=ocamlc

FLAGS=-I ../extlib -I ../swflib
FILES=tTFData tTFParser tTFTools tTFSwfWriter tTFCanvasWriter tTFJsonWriter
LIBS=extLib swflib unix

OUTPUT=ttf

all: native bytecode

native: ttflib.cmxa

bytecode: ttflib.cma

ttflib.cmxa: $(FILES:=.ml)
	$(OCAMLOPT) $(FLAGS) $(FILES:=.ml) -g -a -o ttflib.cmxa

ttflib.cma: $(FILES:=.ml)
	$(OCAMLC) $(FLAGS) $(FILES:=.ml) -g -a -o ttflib.cma

exec:
	$(OCAMLOPT) $(FLAGS) $(LIBS:=.cmxa) $(FILES:=.ml) main.ml -g -o $(OUTPUT)

clean:
	rm -rf ttflib.cmxa ttflib.cma ttflib.lib ttflib.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)

.PHONY: all native bytecode clean exec

Makefile: ;
$(FILES:=.ml): ;
