# Makefile contributed by Alain Frisch

MODULES = \
 enum bitSet dynArray extArray extHashtbl extList extString global IO option \
 pMap std uChar uTF8 base64 unzip refList optParse dllist

# the list is topologically sorted

MLI = $(MODULES:=.mli)
SRC = $(MLI) $(MODULES:=.ml) extLib.ml

all: 
	ocamlc -a -o extLib.cma $(SRC)
opt: 
	ocamlopt -a -o extLib.cmxa $(SRC)
doc:
	ocamlc -c $(MODULES:=.mli)
	mkdir -p doc/
	ocamldoc -sort -html -d doc/ $(MODULES:=.mli)
	cp odoc_style.css doc/style.css

install:
	cp META.txt META
	ocamlfind install extlib META *.cmi *.cma $(MLI) $(wildcard *.cmxa) $(wildcard *.a)

uninstall:
	ocamlfind remove extlib

clean:
	rm -f *.cmo *.cmx *.o *.cmi *.cma *.cmxa *.a
	rm -Rf doc
