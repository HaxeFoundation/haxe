OCAMLOPT=ocamlopt

all: ../zip.cmxa minizip.ml
	$(OCAMLOPT) -g -g -I .. -I ../../extc -o minizip -cclib ../../extc/extc_stubs.o -cclib -lz unix.cmxa ../zip.cmxa minizip.ml

clean:
	rm -rf minizip $(wildcard *.cmx) $(wildcard *.obj) $(wildcard *.o) $(wildcard *.cmi)
