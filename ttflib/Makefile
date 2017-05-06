OCAMLOPT=ocamlopt
OCAMLC=ocamlc

FLAGS=-package extlib -safe-string -I ../extlib-leftovers -I ../swflib
FILES=tTFData tTFParser tTFTools tTFSwfWriter tTFCanvasWriter tTFJsonWriter
LIBS=extLib swflib unix

OUTPUT=ttf

all: native bytecode

native: ttflib.cmxa

bytecode: ttflib.cma

ttflib.cmxa: $(FILES:=.ml)
	ocamlfind $(OCAMLOPT) $(FLAGS) $(FILES:=.ml) -g -a -o ttflib.cmxa

ttflib.cma: $(FILES:=.ml)
	ocamlfind $(OCAMLC) $(FLAGS) $(FILES:=.ml) -g -a -o ttflib.cma

exec:
	ocamlfind $(OCAMLOPT) $(FLAGS) $(LIBS:=.cmxa) $(FILES:=.ml) main.ml -g -o $(OUTPUT)

clean:
	rm -rf ttflib.cmxa ttflib.cma ttflib.lib ttflib.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)

.PHONY: all native bytecode clean exec

Makefile: ;
$(FILES:=.ml): ;
