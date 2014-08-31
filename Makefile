# Haxe compiler Makefile
#
#  - use 'make' to build all
#  - use 'make haxe' to build only the compiler (not the libraries)
#  - if you want to build quickly, install 'ocamlopt.opt' and change OCAMLOPT=ocamlopt.top
#
#  Windows users :
#  - use 'make -f Makefile.win' to build for Windows
#  - use 'make MSVC=1 -f Makefile.win' to build for Windows with OCaml/MSVC
#
.SUFFIXES : .ml .mli .cmo .cmi .cmx .mll .mly

INSTALL_DIR=/usr
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib/haxe

OUTPUT=haxe
EXTENSION=
OCAMLOPT=ocamlopt
OCAMLC=ocamlc
LFLAGS=

CFLAGS= -g -I libs/extlib -I libs/extc -I libs/neko -I libs/javalib -I libs/ziplib -I libs/swflib -I libs/xml-light -I libs/ttflib -I libs/ilib -I libs/objsize

LIBS=unix str libs/extlib/extLib libs/xml-light/xml-light libs/swflib/swflib \
	libs/extc/extc libs/neko/neko libs/javalib/java libs/ziplib/zip \
	libs/ttflib/ttf libs/ilib/il libs/objsize/objsize

NATIVE_LIBS=-cclib libs/extc/extc_stubs.o -cclib -lz -cclib libs/objsize/c_objsize.o

ifeq ($(BYTECODE),1)
	TARGET_FLAG = bytecode
	COMPILER = $(OCAMLC)
	LIB_EXT = cma
	MODULE_EXT = cmo
	NATIVE_LIB_FLAG = -custom
else
	TARGET_FLAG = native
	COMPILER = $(OCAMLOPT)
	LIB_EXT = cmxa
	MODULE_EXT = cmx
endif

CC_CMD = $(COMPILER) $(CFLAGS) -c $<

CC_PARSER_CMD = $(COMPILER) -pp camlp4o $(CFLAGS) -c parser.ml

RELDIR=../../..

MODULES=ast type lexer common genxml parser typecore optimizer typeload \
	codegen gencommon genas3 gencpp genjs genneko genphp genswf8 \
	genswf9 genswf genjava gencs genpy interp dce filters typer matcher version main

ADD_REVISION=0

# using $(CURDIR) on Windows will not work since it might be a Cygwin path
ifdef SYSTEMROOT
	EXTENSION=.exe
else
	export HAXE_STD_PATH=$(CURDIR)/std
endif

ifneq ($(ADD_REVISION),0)
	VERSION_EXTRA="let version_extra = Some \" (git build $(shell git rev-parse --abbrev-ref HEAD) @ $(shell git describe --always)) \""
else
	VERSION_EXTRA="let version_extra = None"
endif

all: libs haxe

libs:
	make -C libs/extlib OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/extc OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/neko OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/javalib OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/ilib OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/ziplib OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/swflib OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/xml-light OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/ttflib OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)
	make -C libs/objsize OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)

haxe: $(MODULES:=.$(MODULE_EXT))
	$(COMPILER) -o $(OUTPUT) $(NATIVE_LIBS) $(NATIVE_LIB_FLAG) $(LFLAGS) $(LIBS:=.$(LIB_EXT)) $(MODULES:=.$(MODULE_EXT))

haxelib:
	(cd $(CURDIR)/extra/haxelib_src && $(CURDIR)/$(OUTPUT) haxelib.hxml && nekotools boot bin/haxelib.n)
	cp extra/haxelib_src/bin/haxelib$(EXTENSION) haxelib$(EXTENSION)

tools: haxelib

install:
	-rm -f $(INSTALL_LIB_DIR)
	-mkdir -p $(INSTALL_LIB_DIR)
	rm -rf $(INSTALL_LIB_DIR)/std
	cp -rf std $(INSTALL_LIB_DIR)/std
	cp -rf extra $(INSTALL_LIB_DIR)
	-mkdir -p $(INSTALL_LIB_DIR)/lib
	rm -f $(INSTALL_BIN_DIR)/haxe
	cp haxe $(INSTALL_LIB_DIR)
	ln -s $(INSTALL_LIB_DIR)/haxe $(INSTALL_BIN_DIR)/haxe
	chmod -R a+rx $(INSTALL_LIB_DIR)
	chmod 777 $(INSTALL_LIB_DIR)/lib
	# cp extra/haxelib_src/haxelib_script.sh $(INSTALL_DIR)/bin/haxelib
	echo "#!/bin/sh" > $(INSTALL_BIN_DIR)/haxelib
	echo "exec haxe -cp $(INSTALL_LIB_DIR)/extra/haxelib_src/src --run tools.haxelib.Main \"\$$@\"" >> $(INSTALL_BIN_DIR)/haxelib
	chmod a+rx $(INSTALL_BIN_DIR)/haxe $(INSTALL_BIN_DIR)/haxelib

# will install native version of the tools instead of script ones
install_tools: tools
	cp haxelib ${INSTALL_BIN_DIR}/haxelib
	chmod a+rx $(INSTALL_BIN_DIR)/haxelib

uninstall:
	rm -rf $(INSTALL_BIN_DIR)/haxe $(INSTALL_BIN_DIR)/haxelib $(INSTALL_LIB_DIR)

# Modules

codegen.$(MODULE_EXT): optimizer.$(MODULE_EXT) typeload.$(MODULE_EXT) typecore.$(MODULE_EXT) type.$(MODULE_EXT) genxml.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

common.$(MODULE_EXT): type.$(MODULE_EXT) ast.$(MODULE_EXT)

dce.$(MODULE_EXT): ast.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) type.$(MODULE_EXT)

filters.$(MODULE_EXT): ast.$(MODULE_EXT) common.$(MODULE_EXT) type.$(MODULE_EXT) dce.$(MODULE_EXT) codegen.$(MODULE_EXT) typecore.$(MODULE_EXT)

genas3.$(MODULE_EXT): type.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

gencommon.$(MODULE_EXT): type.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

gencpp.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

gencs.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) gencommon.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genjava.$(MODULE_EXT): type.$(MODULE_EXT) gencommon.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genjs.$(MODULE_EXT): type.$(MODULE_EXT) optimizer.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genneko.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genphp.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genpy.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genswf.$(MODULE_EXT): type.$(MODULE_EXT) genswf9.$(MODULE_EXT) genswf8.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

genswf8.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genswf9.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) genswf8.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genxml.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

interp.$(MODULE_EXT): typecore.$(MODULE_EXT) type.$(MODULE_EXT) lexer.$(MODULE_EXT) genneko.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) genswf.$(MODULE_EXT) genjava.$(MODULE_EXT) gencs.$(MODULE_EXT) parser.$(MODULE_EXT)

matcher.$(MODULE_EXT): optimizer.$(MODULE_EXT) codegen.$(MODULE_EXT) typecore.$(MODULE_EXT) type.$(MODULE_EXT) typer.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

main.$(MODULE_EXT): filters.$(MODULE_EXT) matcher.$(MODULE_EXT) typer.$(MODULE_EXT) typeload.$(MODULE_EXT) typecore.$(MODULE_EXT) type.$(MODULE_EXT) parser.$(MODULE_EXT) optimizer.$(MODULE_EXT) lexer.$(MODULE_EXT) interp.$(MODULE_EXT) genxml.$(MODULE_EXT) genswf.$(MODULE_EXT) genphp.$(MODULE_EXT) genneko.$(MODULE_EXT) genjs.$(MODULE_EXT) gencpp.$(MODULE_EXT) genas3.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) gencommon.$(MODULE_EXT) genjava.$(MODULE_EXT) gencs.$(MODULE_EXT) genpy.$(MODULE_EXT) version.$(MODULE_EXT)

optimizer.$(MODULE_EXT): typecore.$(MODULE_EXT) type.$(MODULE_EXT) parser.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

parser.$(MODULE_EXT): parser.ml lexer.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)
	$(CC_PARSER_CMD)

type.$(MODULE_EXT): ast.$(MODULE_EXT)

typecore.$(MODULE_EXT): type.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

typeload.$(MODULE_EXT): typecore.$(MODULE_EXT) type.$(MODULE_EXT) parser.$(MODULE_EXT) optimizer.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

typer.$(MODULE_EXT): typeload.$(MODULE_EXT) typecore.$(MODULE_EXT) type.$(MODULE_EXT) parser.$(MODULE_EXT) optimizer.$(MODULE_EXT) lexer.$(MODULE_EXT) interp.$(MODULE_EXT) genneko.$(MODULE_EXT) genjs.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) filters.$(MODULE_EXT)

lexer.$(MODULE_EXT): lexer.ml

lexer.$(MODULE_EXT): ast.$(MODULE_EXT)

ast.$(MODULE_EXT):

version.$(MODULE_EXT):
	echo $(VERSION_EXTRA) > version.ml
	$(COMPILER) $(CFLAGS) -c version.ml

# Clean

clean: clean_libs clean_haxe clean_tools

clean_libs:
	make -C libs/extlib clean
	make -C libs/extc clean
	make -C libs/neko clean
	make -C libs/ziplib clean
	make -C libs/javalib clean
	make -C libs/ilib clean
	make -C libs/swflib clean
	make -C libs/xml-light clean
	make -C libs/ttflib clean
	make -C libs/objsize clean

clean_haxe:
	rm -f $(MODULES:=.obj) $(MODULES:=.o) $(MODULES:=.cmx) $(MODULES:=.cmi) $(MODULES:=.cmo) lexer.ml $(OUTPUT)

clean_tools:
	rm -f $(OUTPUT) haxelib

# SUFFIXES

.ml.cmx:
	$(CC_CMD)

.ml.cmo:
	$(CC_CMD)

.mll.ml:
	ocamllex $<

.PHONY: haxe libs version.cmx version.cmo haxelib
