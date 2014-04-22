OCAMLOPT=ocamlopt
OCAMLC=ocamlc

all: native bytecode

native:
	$(OCAMLOPT) -g -I ../extlib -I ../extc -a -o zip.cmxa zlib.mli zlib.ml zip.mli zip.ml

bytecode:
	$(OCAMLC) -g -I ../extlib -I ../extc -a -o zip.cma zlib.mli zlib.ml zip.mli zip.ml

clean:
	rm -rf zip.cmxa zip.cma zip.lib zip.a $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi) $(wildcard *.cmo)
