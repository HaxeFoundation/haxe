# Haxe compiler Makefile
#
#  - use 'make' to build all
#  - use 'make haxe' to build only the compiler (not the libraries)
#  - if you want to build quickly, install 'ocamlopt.opt' and change OCAMLOPT=ocamlopt.opt
#
#  Windows users :
#  - use 'make -f Makefile.win' to build for Windows
#  - use 'make MSVC=1 -f Makefile.win' to build for Windows with OCaml/MSVC
#
.SUFFIXES : .ml .mli .cmo .cmi .cmx .mll .mly

INSTALL_DIR=$(DESTDIR)/usr/local
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib/haxe
INSTALL_STD_DIR=$(INSTALL_LIB_DIR)/std
PACKAGE_OUT_DIR=out
PACKAGE_SRC_EXTENSION=.tar.gz

OUTPUT=haxe
EXTENSION=
OCAMLOPT?=ocamlopt
OCAMLC?=ocamlc
LFLAGS=

CFLAGS= -g -I libs/extlib -I libs/extc -I libs/neko -I libs/javalib -I libs/ziplib -I libs/swflib -I libs/xml-light -I libs/ttflib -I libs/ilib -I libs/objsize

LIBS=unix str libs/extlib/extLib libs/xml-light/xml-light libs/swflib/swflib \
	libs/extc/extc libs/neko/neko libs/javalib/java libs/ziplib/zip \
	libs/ttflib/ttf libs/ilib/il libs/objsize/objsize

NATIVE_LIBS=-cclib libs/extc/extc_stubs.o -cclib libs/extc/process_stubs.o -cclib -lz -cclib libs/objsize/c_objsize.o

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
	codegen gencommon genas3 gencpp genjs genneko genphp \
	genswf9 genswf genjava gencs genpy interp dce analyzer filters typer matcher version main

ADD_REVISION?=0

BRANCH=$(shell echo $$APPVEYOR_REPO_NAME | grep -q /haxe && echo $$APPVEYOR_REPO_BRANCH || echo $$TRAVIS_REPO_SLUG | grep -q /haxe && echo $$TRAVIS_BRANCH || git rev-parse --abbrev-ref HEAD)
COMMIT_SHA=$(shell git rev-parse --short HEAD)
COMMIT_DATE=$(shell \
	if [ "$$(uname)" = "Darwin" ]; then \
		date -u -r $$(git show -s --format=%ct HEAD) +%Y%m%d%H%M%S; \
	else \
		date -u -d @$$(git show -s --format=%ct HEAD) +%Y%m%d%H%M%S; \
	fi \
)
PACKAGE_FILE_NAME=haxe_$(COMMIT_DATE)_$(COMMIT_SHA)

# using $(CURDIR) on Windows will not work since it might be a Cygwin path
ifdef SYSTEMROOT
	EXTENSION=.exe
else
	export HAXE_STD_PATH=$(CURDIR)/std
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
	rm -rf $(INSTALL_LIB_DIR)
	mkdir -p $(INSTALL_BIN_DIR)
	mkdir -p $(INSTALL_LIB_DIR)/lib
	rm -rf $(INSTALL_STD_DIR)
	cp -rf std $(INSTALL_STD_DIR)
	cp -rf extra $(INSTALL_LIB_DIR)
	rm -f $(INSTALL_BIN_DIR)/haxe
	cp haxe $(INSTALL_LIB_DIR)
	ln -s $(INSTALL_LIB_DIR)/haxe $(INSTALL_BIN_DIR)/haxe
	chmod -R a+rx $(INSTALL_LIB_DIR)
	chmod 777 $(INSTALL_LIB_DIR)/lib
	# cp extra/haxelib_src/haxelib_script.sh $(INSTALL_DIR)/bin/haxelib
	echo "#!/bin/sh" > $(INSTALL_BIN_DIR)/haxelib
	echo "exec haxe -cp $(INSTALL_LIB_DIR)/extra/haxelib_src/src --run haxelib.client.Main \"\$$@\"" >> $(INSTALL_BIN_DIR)/haxelib
	chmod a+rx $(INSTALL_BIN_DIR)/haxe $(INSTALL_BIN_DIR)/haxelib

# will install native version of the tools instead of script ones
install_tools: tools
	cp haxelib ${INSTALL_BIN_DIR}/haxelib
	chmod a+rx $(INSTALL_BIN_DIR)/haxelib

uninstall:
	rm -rf $(INSTALL_BIN_DIR)/haxe $(INSTALL_BIN_DIR)/haxelib $(INSTALL_LIB_DIR)

# Modules

analyzer.$(MODULE_EXT): ast.$(MODULE_EXT) type.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT)

codegen.$(MODULE_EXT): optimizer.$(MODULE_EXT) typeload.$(MODULE_EXT) typecore.$(MODULE_EXT) type.$(MODULE_EXT) genxml.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

common.$(MODULE_EXT): type.$(MODULE_EXT) ast.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

dce.$(MODULE_EXT): ast.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) type.$(MODULE_EXT)

filters.$(MODULE_EXT): ast.$(MODULE_EXT) analyzer.$(MODULE_EXT) common.$(MODULE_EXT) type.$(MODULE_EXT) dce.$(MODULE_EXT) codegen.$(MODULE_EXT) typecore.$(MODULE_EXT)

genas3.$(MODULE_EXT): type.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

gencommon.$(MODULE_EXT): type.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

gencpp.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) gencommon.$(MODULE_EXT)

gencs.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) gencommon.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

genjava.$(MODULE_EXT): type.$(MODULE_EXT) gencommon.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genjs.$(MODULE_EXT): type.$(MODULE_EXT) optimizer.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genneko.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genphp.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genpy.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genswf.$(MODULE_EXT): type.$(MODULE_EXT) genswf9.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

genswf9.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT)

genxml.$(MODULE_EXT): type.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

interp.$(MODULE_EXT): typecore.$(MODULE_EXT) type.$(MODULE_EXT) lexer.$(MODULE_EXT) genneko.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) genswf.$(MODULE_EXT) genjava.$(MODULE_EXT) gencs.$(MODULE_EXT) parser.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

matcher.$(MODULE_EXT): optimizer.$(MODULE_EXT) codegen.$(MODULE_EXT) typecore.$(MODULE_EXT) type.$(MODULE_EXT) typer.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

main.$(MODULE_EXT): filters.$(MODULE_EXT) matcher.$(MODULE_EXT) typer.$(MODULE_EXT) typeload.$(MODULE_EXT) typecore.$(MODULE_EXT) type.$(MODULE_EXT) parser.$(MODULE_EXT) optimizer.$(MODULE_EXT) lexer.$(MODULE_EXT) interp.$(MODULE_EXT) genxml.$(MODULE_EXT) genswf.$(MODULE_EXT) genphp.$(MODULE_EXT) genneko.$(MODULE_EXT) genjs.$(MODULE_EXT) gencpp.$(MODULE_EXT) genas3.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) gencommon.$(MODULE_EXT) genjava.$(MODULE_EXT) gencs.$(MODULE_EXT) genpy.$(MODULE_EXT) version.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

optimizer.$(MODULE_EXT): typecore.$(MODULE_EXT) type.$(MODULE_EXT) parser.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

parser.$(MODULE_EXT): parser.ml lexer.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)
	$(CC_PARSER_CMD)

type.$(MODULE_EXT): ast.$(MODULE_EXT)

typecore.$(MODULE_EXT): type.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

typeload.$(MODULE_EXT): typecore.$(MODULE_EXT) type.$(MODULE_EXT) parser.$(MODULE_EXT) optimizer.$(MODULE_EXT) lexer.$(MODULE_EXT) common.$(MODULE_EXT) ast.$(MODULE_EXT)

typer.$(MODULE_EXT): typeload.$(MODULE_EXT) typecore.$(MODULE_EXT) type.$(MODULE_EXT) parser.$(MODULE_EXT) optimizer.$(MODULE_EXT) lexer.$(MODULE_EXT) interp.$(MODULE_EXT) genneko.$(MODULE_EXT) genjs.$(MODULE_EXT) common.$(MODULE_EXT) codegen.$(MODULE_EXT) ast.$(MODULE_EXT) filters.$(MODULE_EXT) gencommon.$(MODULE_EXT)

lexer.$(MODULE_EXT): lexer.ml

lexer.$(MODULE_EXT): ast.$(MODULE_EXT)

ast.$(MODULE_EXT):

version.$(MODULE_EXT):
	$(MAKE) -f Makefile.version_extra -s --no-print-directory ADD_REVISION=$(ADD_REVISION) BRANCH=$(BRANCH) COMMIT_SHA=$(COMMIT_SHA) COMMIT_DATE=$(COMMIT_DATE) > version.ml
	$(COMPILER) $(CFLAGS) -c version.ml

# Package

package_src:
	mkdir -p $(PACKAGE_OUT_DIR)
	# use git-archive-all since we have submodules
	# https://github.com/Kentzo/git-archive-all
	curl -s https://raw.githubusercontent.com/Kentzo/git-archive-all/1.12/git-archive-all -o extra/git-archive-all
	python extra/git-archive-all $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_src$(PACKAGE_SRC_EXTENSION)

package_bin:
	mkdir -p $(PACKAGE_OUT_DIR)
	rm -rf $(PACKAGE_FILE_NAME) $(PACKAGE_FILE_NAME).tar.gz
	# Copy the package contents to $(PACKAGE_FILE_NAME)
	mkdir -p $(PACKAGE_FILE_NAME)
	cp -r $(OUTPUT) haxelib$(EXTENSION) std extra/LICENSE.txt extra/CONTRIB.txt extra/CHANGES.txt $(PACKAGE_FILE_NAME)
	# archive
	tar -zcf $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_bin.tar.gz $(PACKAGE_FILE_NAME)
	rm -r $(PACKAGE_FILE_NAME)


install_dox:
	haxelib git hxparse https://github.com/Simn/hxparse development src
	haxelib git hxtemplo https://github.com/Simn/hxtemplo
	haxelib git hxargs https://github.com/Simn/hxargs
	haxelib git markdown https://github.com/dpeek/haxe-markdown master src
	haxelib git hxcpp https://github.com/HaxeFoundation/hxcpp
	haxelib git hxjava https://github.com/HaxeFoundation/hxjava
	haxelib git hxcs https://github.com/HaxeFoundation/hxcs
	haxelib git dox https://github.com/dpeek/dox

package_doc:
	cd $$(haxelib path dox | head -n 1) && haxe run.hxml && haxe gen.hxml
	haxelib run dox --title "Haxe API" -o $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_doc.zip -D version "$$(haxe -version 2>&1)" -i $$(haxelib path dox | head -n 1)bin/xml -ex microsoft -ex javax -ex cs.internal -D source-path https://github.com/HaxeFoundation/haxe/blob/$(BRANCH)/std/

# Clean

clean: clean_libs clean_haxe clean_tools clean_package

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
	rm -f $(MODULES:=.obj) $(MODULES:=.o) $(MODULES:=.cmx) $(MODULES:=.cmi) $(MODULES:=.cmo) lexer.ml version.ml $(OUTPUT)

clean_tools:
	rm -f $(OUTPUT) haxelib
	rm -f extra/haxelib_src/bin/haxelib.n
	rm -f extra/haxelib_src/bin/haxelib

clean_package:
	rm -rf $(PACKAGE_OUT_DIR)

# SUFFIXES

.ml.cmx:
	$(CC_CMD)

.ml.cmo:
	$(CC_CMD)

.mll.ml:
	ocamllex $<

.PHONY: haxe libs version.cmx version.cmo haxelib
