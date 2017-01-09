OCAMLOPT = ocamlopt
OCAMLC = ocamlc
TARGET_FLAG = all
LIBS=extlib extc neko javalib ilib ziplib swflib xml-light ttflib objsize pcre

all: $(LIBS)
$(LIBS):
	make -C $@ OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)

clean:
	make -C extlib clean
	make -C extc clean
	make -C neko clean
	make -C ziplib clean
	make -C javalib clean
	make -C ilib clean
	make -C swflib clean
	make -C xml-light clean
	make -C ttflib clean
	make -C objsize clean
	make -C pcre clean

.PHONY: all clean $(LIBS)

Makefile: ;
