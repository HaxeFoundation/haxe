OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC=zlib.mli zlib.ml zip.mli zip.ml

all: native bytecode

native: zip.cmxa
zip.cmxa: $(SRC)
	$(OCAMLOPT) -g -I ../extlib -I ../extc -a -o zip.cmxa $(SRC)

bytecode: zip.cma
zip.cma: $(SRC)
	$(OCAMLC) -g -I ../extlib -I ../extc -a -o zip.cma $(SRC)

clean:
	rm -rf zip.cmxa zip.cma zip.lib zip.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)

.PHONY: all native bytecode clean

Makefile: ;

$(SRC): ;
