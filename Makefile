EXTLIB=../../mtcvs/extlib-dev
SWFLIB=../../mtcvs/swflib
EXTC=../../mtcvs/extc
NEKO=../neko
FILES=$(EXTLIB)/*.ml* -n $(EXTLIB)/install.ml $(SWFLIB)/*.ml* $(EXTC)/extc.ml* $(NEKO)/libs/include/ocaml/*.ml* *.ml*
LIBS=unix.cmxa
FLAGS=-o haxe -pp camlp4o -lp "-cclib extc_stubs.o -cclib -lz"

all:
	ocamlc -c $(EXTC)/extc_stubs.c
	ocamake $(FLAGS) $(FILES) $(LIBS)

tools:
	(cd std/tools/haxedoc && haxe haxedoc.hxml && cp haxedoc ../../..)
	(cd std/tools/haxelib && haxe haxelib.hxml && cp haxelib ../../..) 

clean:
	ocamake $(FLAGS) -clean $(FILES) $(LIBS)
	rm -rf extc_stubs.o
