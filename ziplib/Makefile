OCAMLOPT=ocamlopt
OCAMLC=ocamlc
SRC=zlib.mli zlib.ml zip.mli zip.ml

all: native bytecode

native: ziplib.cmxa
ziplib.cmxa: $(SRC)
	ocamlfind $(OCAMLOPT) -safe-string -g -I ../extlib -I ../extc -a -o ziplib.cmxa $(SRC)

bytecode: ziplib.cma
ziplib.cma: $(SRC)
	ocamlfind $(OCAMLC) -safe-string -g -I ../extlib -I ../extc -a -o ziplib.cma $(SRC)

clean:
	rm -rf ziplib.cmxa ziplib.cma ziplib.lib ziplib.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)

.PHONY: all native bytecode clean

Makefile: ;

$(SRC): ;
