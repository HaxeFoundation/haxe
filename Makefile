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

INSTALL_DIR=$(DESTDIR)/usr
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib/haxe
INSTALL_STD_DIR=$(INSTALL_LIB_DIR)/std
PACKAGE_OUT_DIR=out
PACKAGE_SRC_EXTENSION=.tar.gz

PLATFORM?=unix

OUTPUT=haxe
EXTENSION=
OCAMLOPT?=ocamlopt
OCAMLC?=ocamlc
LFLAGS=

CFLAGS= -bin-annot
ALL_CFLAGS= $(CFLAGS) -g -w -3 -I libs/extlib -I libs/extc -I libs/neko -I libs/javalib -I libs/ziplib -I libs/swflib -I libs/xml-light -I libs/ttflib -I libs/ilib -I libs/objsize \
	-I src -I src/context -I src/generators -I src/macro -I src/optimization -I src/syntax -I src/typing -I src/display

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

CC_CMD = $(COMPILER) $(ALL_CFLAGS) -c $<

CC_PARSER_CMD = $(COMPILER) -pp camlp4o $(ALL_CFLAGS) -c src/syntax/parser.ml

RELDIR=../../..

MODULES=json version globals path context/meta syntax/ast display/displayTypes typing/type typing/error \
	syntax/lexer context/common generators/genxml \
	syntax/parser typing/abstract typing/typecore display/display optimization/optimizerTexpr \
	optimization/optimizer typing/overloads typing/typeload generators/codegen generators/gencommon generators/genas3 \
	generators/gencpp generators/genjs generators/genneko generators/genphp generators/genswf9 \
	generators/genswf generators/genjava generators/gencs generators/genpy macro/interp generators/hlcode generators/hlopt generators/hlinterp generators/hl2c generators/genhl \
	generators/genlua \
	optimization/dce optimization/analyzerConfig optimization/analyzerTypes optimization/analyzerTexpr \
	optimization/analyzerTexprTransformer optimization/analyzer \
	optimization/filters typing/typer typing/matcher display/displayOutput server main

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

all: libs haxe tools

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

haxe: $(MODULES:%=src/%.$(MODULE_EXT))
	$(COMPILER) -o $(OUTPUT) $(NATIVE_LIBS) $(NATIVE_LIB_FLAG) $(LFLAGS) $(LIBS:=.$(LIB_EXT)) $(MODULES:%=src/%.$(MODULE_EXT))

haxelib:
	(cd $(CURDIR)/extra/haxelib_src && $(CURDIR)/$(OUTPUT) client.hxml && nekotools boot run.n)
	mv extra/haxelib_src/run$(EXTENSION) haxelib$(EXTENSION)

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
	rm -f $(INSTALL_BIN_DIR)/haxelib
	cp haxelib $(INSTALL_LIB_DIR)
	ln -s $(INSTALL_LIB_DIR)/haxelib $(INSTALL_BIN_DIR)/haxelib
	chmod -R a+rx $(INSTALL_LIB_DIR)
	chmod 777 $(INSTALL_LIB_DIR)/lib
	chmod a+rx $(INSTALL_BIN_DIR)/haxe $(INSTALL_BIN_DIR)/haxelib

# will install native version of the tools instead of script ones
install_tools: tools
	cp haxelib ${INSTALL_BIN_DIR}/haxelib
	chmod a+rx $(INSTALL_BIN_DIR)/haxelib

uninstall:
	rm -rf $(INSTALL_BIN_DIR)/haxe $(INSTALL_BIN_DIR)/haxelib $(INSTALL_LIB_DIR)

# Modules

# context

src/context/common.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/display/displayTypes.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

src/context/meta.$(MODULE_EXT): src/globals.$(MODULE_EXT)

# display

src/display/display.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/path.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/syntax/parser.$(MODULE_EXT) src/display/displayTypes.$(MODULE_EXT)

src/display/displayTypes.$(MODULE_EXT) : src/globals.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/display/displayOutput.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/typing/typer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/display/display.$(MODULE_EXT)

# generators

src/generators/codegen.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/generators/genxml.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/genas3.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/gencommon.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/typing/overloads.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

src/generators/gencpp.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/gencs.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/generators/gencommon.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

src/generators/genjava.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/generators/gencommon.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/genjs.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/genneko.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/genlua.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/genphp.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/genpy.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/genswf.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/generators/genswf9.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/hlinterp.$(MODULE_EXT): src/context/common.$(MODULE_EXT) src/generators/hlcode.$(MODULE_EXT) src/macro/interp.$(MODULE_EXT) src/generators/hlopt.$(MODULE_EXT)

src/generators/hl2c.$(MODULE_EXT): src/generators/hlcode.$(MODULE_EXT)

src/generators/hlopt.$(MODULE_EXT): src/generators/hlcode.$(MODULE_EXT)

src/generators/genhl.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/generators/hlcode.$(MODULE_EXT) src/generators/hlinterp.$(MODULE_EXT) src/generators/hl2c.$(MODULE_EXT) src/generators/hlopt.$(MODULE_EXT)

src/generators/genswf9.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/generators/genxml.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

# macro

src/macro/interp.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/generators/genneko.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/generators/genswf.$(MODULE_EXT) src/generators/genjava.$(MODULE_EXT) src/generators/gencs.$(MODULE_EXT) src/syntax/parser.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

# optimization

src/optimization/analyzer.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/optimization/optimizer.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/optimization/analyzerConfig.$(MODULE_EXT) src/optimization/analyzerTypes.$(MODULE_EXT) src/optimization/analyzerTexpr.$(MODULE_EXT) src/optimization/analyzerTexprTransformer.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT)

src/optimization/analyzerConfig.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT)

src/optimization/analyzerTexpr.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/optimization/analyzerConfig.$(MODULE_EXT) src/optimization/optimizerTexpr.$(MODULE_EXT)

src/optimization/analyzerTexprTransformer.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/optimization/analyzerConfig.$(MODULE_EXT) src/optimization/analyzerTypes.$(MODULE_EXT) src/optimization/analyzerTexpr.$(MODULE_EXT)

src/optimization/analyzerTypes.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/optimization/analyzerConfig.$(MODULE_EXT)

src/optimization/dce.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/path.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/typing/type.$(MODULE_EXT)

src/optimization/filters.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/optimization/analyzer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/optimization/dce.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT)

src/optimization/optimizer.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/optimization/optimizerTexpr.$(MODULE_EXT) src/display/display.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/parser.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/optimization/optimizerTexpr.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

# syntax

src/syntax/ast.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT)

src/syntax/lexer.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/syntax/lexer.ml src/syntax/ast.$(MODULE_EXT)

src/syntax/parser.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/path.$(MODULE_EXT) src/syntax/parser.ml src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)
	$(CC_PARSER_CMD)

# typing

src/typing/abstract.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/typing/error.$(MODULE_EXT)

src/typing/error.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/typing/type.$(MODULE_EXT)

src/typing/matcher.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/optimization/optimizer.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/typing/typer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/typing/overloads.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/typing/type.$(MODULE_EXT)

src/typing/type.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/json.$(MODULE_EXT)

src/typing/typecore.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT)

src/typing/typeload.$(MODULE_EXT): src/globals.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/optimization/optimizerTexpr.$(MODULE_EXT) src/typing/overloads.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/parser.$(MODULE_EXT) src/optimization/optimizer.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/json.$(MODULE_EXT) src/display/display.$(MODULE_EXT)

src/typing/typer.$(MODULE_EXT): src/typing/abstract.$(MODULE_EXT) src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/optimization/optimizerTexpr.$(MODULE_EXT) src/typing/overloads.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/typeload.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/parser.$(MODULE_EXT) src/optimization/optimizer.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/macro/interp.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/optimization/filters.$(MODULE_EXT) src/generators/genjs.$(MODULE_EXT) src/display/display.$(MODULE_EXT)

# main

src/main.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/typing/error.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/path.$(MODULE_EXT) src/optimization/filters.$(MODULE_EXT) src/typing/matcher.$(MODULE_EXT) src/typing/typer.$(MODULE_EXT) src/typing/typeload.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/parser.$(MODULE_EXT) src/optimization/optimizer.$(MODULE_EXT) src/syntax/lexer.$(MODULE_EXT) src/macro/interp.$(MODULE_EXT) src/generators/genxml.$(MODULE_EXT) src/generators/genswf.$(MODULE_EXT) src/generators/genphp.$(MODULE_EXT) src/generators/genneko.$(MODULE_EXT) src/generators/genjs.$(MODULE_EXT) src/generators/genlua.$(MODULE_EXT) src/generators/gencpp.$(MODULE_EXT) src/generators/genas3.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/generators/codegen.$(MODULE_EXT) src/generators/genjava.$(MODULE_EXT) src/generators/gencs.$(MODULE_EXT) src/generators/genpy.$(MODULE_EXT) src/generators/genhl.$(MODULE_EXT) src/display/display.$(MODULE_EXT) src/server.$(MODULE_EXT) src/display/displayOutput.$(MODULE_EXT) libs/ilib/il.$(LIB_EXT)

src/globals.$(MODULE_EXT): src/version.$(MODULE_EXT)

src/path.$(MODULE_EXT): src/globals.$(MODULE_EXT)

src/server.$(MODULE_EXT): src/context/meta.$(MODULE_EXT) src/globals.$(MODULE_EXT) src/path.$(MODULE_EXT) src/typing/typer.$(MODULE_EXT) src/typing/typeload.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/syntax/parser.$(MODULE_EXT) src/typing/typecore.$(MODULE_EXT) src/typing/type.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/context/common.$(MODULE_EXT) src/syntax/parser.$(MODULE_EXT) src/syntax/ast.$(MODULE_EXT) src/display/displayOutput.$(MODULE_EXT)

src/version.$(MODULE_EXT):
	$(MAKE) -f Makefile.version_extra -s --no-print-directory ADD_REVISION=$(ADD_REVISION) BRANCH=$(BRANCH) COMMIT_SHA=$(COMMIT_SHA) COMMIT_DATE=$(COMMIT_DATE) > src/version.ml
	$(COMPILER) $(ALL_CFLAGS) -c src/version.ml

# Package

package_src:
	mkdir -p $(PACKAGE_OUT_DIR)
	# use git-archive-all since we have submodules
	# https://github.com/Kentzo/git-archive-all
	curl -s https://raw.githubusercontent.com/Kentzo/git-archive-all/1.12/git-archive-all -o extra/git-archive-all
	python extra/git-archive-all $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_src$(PACKAGE_SRC_EXTENSION)

package_unix:
	mkdir -p $(PACKAGE_OUT_DIR)
	rm -rf $(PACKAGE_FILE_NAME) $(PACKAGE_FILE_NAME).tar.gz
	# Copy the package contents to $(PACKAGE_FILE_NAME)
	mkdir -p $(PACKAGE_FILE_NAME)
	cp -r $(OUTPUT) haxelib$(EXTENSION) std extra/LICENSE.txt extra/CONTRIB.txt extra/CHANGES.txt $(PACKAGE_FILE_NAME)
	# archive
	tar -zcf $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_bin.tar.gz $(PACKAGE_FILE_NAME)
	rm -r $(PACKAGE_FILE_NAME)

package_bin: package_$(PLATFORM)

install_dox:
	haxelib git hxparse https://github.com/Simn/hxparse development src
	haxelib git hxtemplo https://github.com/Simn/hxtemplo
	haxelib git hxargs https://github.com/Simn/hxargs
	haxelib git markdown https://github.com/dpeek/haxe-markdown master src
	haxelib git hxcpp https://github.com/HaxeFoundation/hxcpp
	haxelib git hxjava https://github.com/HaxeFoundation/hxjava
	haxelib git hxcs https://github.com/HaxeFoundation/hxcs
	haxelib git dox https://github.com/HaxeFoundation/dox

package_doc:
	mkdir -p $(PACKAGE_OUT_DIR)
	cd $$(haxelib path dox | head -n 1) && haxe run.hxml && haxe gen.hxml
	haxelib run dox -theme haxe_api -D website "http://haxe.org/" --title "Haxe API" -o $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_doc.zip -D version "$$(haxe -version 2>&1)" -i $$(haxelib path dox | head -n 1)bin/xml -ex microsoft -ex javax -ex cs.internal -D source-path https://github.com/HaxeFoundation/haxe/blob/$(BRANCH)/std/

deploy_doc:
	scp $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_doc.zip www-haxe@api.haxe.org:/data/haxeapi/www/v/dev/api-latest.zip
	ssh www-haxe@api.haxe.org "cd /data/haxeapi/www/v/dev && find . ! -name 'api-latest.zip' -maxdepth 1 -mindepth 1 -exec rm -rf {} + && unzip -q -o api-latest.zip"

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
	rm -f -r  $(MODULES:%=src/%.obj) $(MODULES:%=src/%.o) $(MODULES:%=src/%.cmx) $(MODULES:%=src/%.cmi) $(MODULES:%=src/%.cmo) $(MODULES:%=src/%.cmt) src/syntax/lexer.ml src/version.ml $(OUTPUT)

clean_tools:
	rm -f $(OUTPUT) haxelib

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
