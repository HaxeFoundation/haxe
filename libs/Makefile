OCAMLOPT = ocamlopt
OCAMLC = ocamlc
TARGET_FLAG = all
LIBS=extlib-leftovers extc neko javalib ilib swflib ttflib objsize pcre2 ziplib

all: $(LIBS)
$(LIBS):
	$(MAKE) -C $@ OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)

clean:
	$(MAKE) -C extlib-leftovers clean
	$(MAKE) -C extc clean
	$(MAKE) -C neko clean
	$(MAKE) -C javalib clean
	$(MAKE) -C ilib clean
	$(MAKE) -C swflib clean
	$(MAKE) -C ttflib clean
	$(MAKE) -C objsize clean
	$(MAKE) -C pcre2 clean
	$(MAKE) -C ziplib clean

.PHONY: all clean $(LIBS)

Makefile: ;
