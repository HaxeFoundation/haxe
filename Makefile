OCAMLOPT = ocamlopt
OCAMLC = ocamlc
TARGET_FLAG = all
LIBS=extlib extc neko javalib ilib ziplib swflib xml-light ttflib objsize pcre

all: $(LIBS)
$(LIBS):
	$(MAKE) -C $@ OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)

clean:
	$(MAKE) -C extlib clean
	$(MAKE) -C extc clean
	$(MAKE) -C neko clean
	$(MAKE) -C ziplib clean
	$(MAKE) -C javalib clean
	$(MAKE) -C ilib clean
	$(MAKE) -C swflib clean
	$(MAKE) -C xml-light clean
	$(MAKE) -C ttflib clean
	$(MAKE) -C objsize clean
	$(MAKE) -C pcre clean

.PHONY: all clean $(LIBS)

Makefile: ;
