# Makefile contributed by Alain Frisch

MODULES = \
 enum bitSet dynArray extArray extHashtbl extList extString global IO option \
 pMap std uChar uTF8 base64 unzip refList optParse dllist multiArray

# the list is topologically sorted

MLI = $(MODULES:=.mli)
SRC = $(MLI) $(MODULES:=.ml) extLib.ml

all:
	ocamlc -a -o extLib.cma $(SRC)
opt:
	ocamlopt -g -a -o extLib.cmxa $(SRC)
doc:
	ocamlc -c $(MODULES:=.mli)
	mkdir -p doc/
	ocamldoc -sort -html -d doc/ $(MODULES:=.mli)
	cp odoc_style.css doc/style.css

copy:
	mv *.cmi *.cmx *.cma *.cmxa extLib.lib c:/ocaml/lib/

install:
	cp META.txt META
	ocamlfind install extlib META *.cmi *.cma $(MLI) $(wildcard *.cmxa) $(wildcard *.a)

uninstall:
	ocamlfind remove extlib

clean:
	rm -f $(wildcard *.cmo) $(wildcard *.cmx) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cma) $(wildcard *.cmxa) $(wildcard *.a) $(wildcard *.lib) $(wildcard *.obj)
	rm -Rf doc
