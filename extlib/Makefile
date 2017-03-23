# Makefile contributed by Alain Frisch
OCAMLOPT=ocamlopt
OCAMLC=ocamlc

MODULES = \
 enum bitSet dynArray extArray extHashtbl extList extString global rbuffer IO option \
 pMap std uChar uTF8 base64 unzip refList optParse dllist multiArray sMap

# the list is topologically sorted

MLI = $(MODULES:=.mli)
SRC = $(MLI) $(MODULES:=.ml) extLib.ml

all: bytecode native

opt: native

bytecode: extlib.cma

native: extlib.cmxa

extlib.cma: $(SRC)
	$(OCAMLC) -a -o extlib.cma $(SRC)

extlib.cmxa: $(SRC)
	$(OCAMLOPT) -g -a -o extlib.cmxa $(SRC)

doc:
	$(OCAMLC) -c $(MODULES:=.mli)
	mkdir -p doc/
	ocamldoc -sort -html -d doc/ $(MODULES:=.mli)
	cp odoc_style.css doc/style.css

copy:
	mv *.cmi *.cmx *.cma *.cmxa extlib.lib c:/ocaml/lib/

install:
	cp META.txt META
	ocamlfind install extlib META *.cmi *.cma $(MLI) $(wildcard *.cmxa) $(wildcard *.a)

uninstall:
	ocamlfind remove extlib

clean:
	rm -f $(wildcard *.cmo) $(wildcard *.cmx) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cma) $(wildcard *.cmxa) $(wildcard *.a) $(wildcard *.lib) $(wildcard *.obj)
	rm -Rf doc

.PHONY: all opt bytecode native doc copy install uninstall clean

Makefile: ;

$(SRC): ;
