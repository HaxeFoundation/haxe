EXTLIB=../../mtcvs/extlib-dev
SWFLIB=../../mtcvs/swflib
EXTC=../../mtcvs/extc
NEKO=../neko
XML=../../mtcvs/xml-light
LIBS_SRC=$(EXTLIB)/*.ml* -n $(EXTLIB)/install.ml $(SWFLIB)/*.ml* $(EXTC)/extc.ml*
SRC=$(NEKO)/libs/include/ocaml/*.ml* *.ml*
LIBS=unix.cmxa $(XML)/xml-light.cmxa
FLAGS=-o haxe -pp camlp4o -P $(XML)/dtd.mli -lp "-cclib extc_stubs.o -cclib -lz"
LFLAGS=

ifeq ($(PPC),1)
LFLAGS=-ccopt '-arch ppc'
endif

all: xml
	ocamlopt $(LFLAGS) -c $(EXTC)/extc_stubs.c
	ocamake -lp "$(LFLAGS)" $(FLAGS) $(LIBS_SRC) $(SRC) $(LIBS)

xml:
	(cd ${XML} && make clean xml-light.cmxa)

mode_ppc:	
	sudo ln -sfh /usr/local/bin/ocamlopt.ppc /usr/local/bin/ocamlopt
	sudo ln -sfh /usr/local/lib/ocaml_ppc /usr/local/lib/ocaml

mode_intel:
	sudo ln -sfh /usr/local/bin/ocamlopt.intel /usr/local/bin/ocamlopt
	sudo ln -sfh /usr/local/lib/ocaml_intel /usr/local/lib/ocaml

universal:
	make PPC=1 clean mode_ppc all
	mv haxe haxe.ppc
	make clean mode_intel all
	mv haxe haxe.intel
	lipo -create -arch i386 haxe.intel -arch ppc haxe.ppc -output haxe
	chmod +x haxe

tools:
	(cd std/tools/haxedoc && haxe haxedoc.hxml && cp haxedoc ../../..)
	(cd std/tools/haxelib && haxe haxelib.hxml && cp haxelib ../../..)

clean:
	ocamake $(FLAGS) -clean $(LIBS_SRC) $(SRC) $(LIBS)
	rm -rf extc_stubs.o
