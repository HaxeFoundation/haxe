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
.SUFFIXES : .ml .mli .cmo .cmi .cmx .mly

INSTALL_DIR=$(DESTDIR)/usr
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib/haxe
INSTALL_STD_DIR=$(INSTALL_LIB_DIR)/std
PACKAGE_OUT_DIR=out
PACKAGE_SRC_EXTENSION=.tar.gz

MAKEFILENAME?=Makefile
PLATFORM?=unix

OUTPUT=haxe
EXTENSION=
LFLAGS=
STATICLINK?=0

# Configuration

HAXE_DIRECTORIES=compiler context generators generators/gencommon macro filters optimization syntax typing display
EXTLIB_LIBS=extlib extc neko javalib swflib ttflib ilib objsize pcre
FINDLIB_LIBS=unix str threads sedlex camlzip xml-light

# Includes, packages and compiler

HAXE_INCLUDES=$(HAXE_DIRECTORIES:%=-I _build/src/%)
EXTLIB_INCLUDES=$(EXTLIB_LIBS:%=-I libs/%)
ALL_INCLUDES=$(EXTLIB_INCLUDES) $(HAXE_INCLUDES)
FINDLIB_PACKAGES=$(FINDLIB_LIBS:%=-package %)
CFLAGS=
ALL_CFLAGS=-bin-annot -thread -g -w -3 $(CFLAGS) $(ALL_INCLUDES) $(FINDLIB_PACKAGES)

ifeq ($(BYTECODE),1)
	TARGET_FLAG = bytecode
	COMPILER = ocamlfind ocamlc
	LIB_EXT = cma
	MODULE_EXT = cmo
	NATIVE_LIB_FLAG = -custom
else
	TARGET_FLAG = native
	COMPILER = ocamlfind ocamlopt
	LIB_EXT = cmxa
	MODULE_EXT = cmx
endif

CC_CMD = $(COMPILER) $(ALL_CFLAGS) -c $<

# Meta information

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
HAXE_VERSION=$(shell $(OUTPUT) -version 2>&1 | awk '{print $$1;}')

# using $(CURDIR) on Windows will not work since it might be a Cygwin path
ifdef SYSTEMROOT
	EXTENSION=.exe
else
	export HAXE_STD_PATH=$(CURDIR)/std
endif

# Native libraries

ifneq ($(STATICLINK),0)
	LIB_PARAMS= -cclib '-Wl,-Bstatic -lpcre -lz -Wl,-Bdynamic '
else
	LIB_PARAMS?= -cclib -lpcre -cclib -lz
endif

NATIVE_LIBS=-thread -cclib libs/extc/extc_stubs.o -cclib libs/extc/process_stubs.o -cclib libs/objsize/c_objsize.o -cclib libs/pcre/pcre_stubs.o -ccopt -L/usr/local/lib $(LIB_PARAMS)

# Modules

-include Makefile.modules

# Rules

all: libs haxe tools

libs:
	$(foreach lib,$(EXTLIB_LIBS),$(MAKE) -C libs/$(lib) $(TARGET_FLAG) &&) true

copy_output_files:
	mkdir -p _build
	$(foreach dir,$(HAXE_DIRECTORIES:%=src/%),mkdir -p _build/$(dir) && rsync -u $(dir)/*.ml _build/$(dir) &&) true
	sh compile.sh $(ADD_REVISION)
ifneq ($(ADD_REVISION),0)
	$(MAKE) -f Makefile.version_extra -s --no-print-directory ADD_REVISION=$(ADD_REVISION) BRANCH=$(BRANCH) COMMIT_SHA=$(COMMIT_SHA) COMMIT_DATE=$(COMMIT_DATE) > _build/src/compiler/version.ml
endif

haxe: copy_output_files
	$(MAKE) -f $(MAKEFILENAME) build_pass_1
	$(MAKE) -f $(MAKEFILENAME) build_pass_2
	$(MAKE) -f $(MAKEFILENAME) build_pass_3
	$(MAKE) -f $(MAKEFILENAME) build_pass_4

build_pass_1:
	printf MODULES= > Makefile.modules
	ls -1 $(HAXE_DIRECTORIES:%=_build/src/%/*.ml) | tr '\n' ' ' >> Makefile.modules

build_pass_2:
	printf MODULES= > Makefile.modules
	ocamlfind ocamldep -sort -slash $(HAXE_INCLUDES) $(MODULES) | sed -e "s/\.ml//g" >> Makefile.modules

build_pass_3:
	ocamlfind ocamldep -slash -native $(HAXE_INCLUDES) $(MODULES:%=%.ml) > Makefile.dependencies

build_pass_4: $(MODULES:%=%.$(MODULE_EXT))
	$(COMPILER) -linkpkg -o $(OUTPUT) $(NATIVE_LIBS) $(NATIVE_LIB_FLAG) $(LFLAGS) $(FINDLIB_PACKAGES) $(EXTLIB_INCLUDES) $(EXTLIB_LIBS:=.$(LIB_EXT)) $(MODULES:%=%.$(MODULE_EXT))

haxelib:
	(cd $(CURDIR)/extra/haxelib_src && $(CURDIR)/$(OUTPUT) client.hxml && nekotools boot run.n)
	mv extra/haxelib_src/run$(EXTENSION) haxelib$(EXTENSION)

tools: haxelib

install: uninstall
	mkdir -p $(INSTALL_BIN_DIR)
	mkdir -p $(INSTALL_LIB_DIR)/lib
	cp -rf std $(INSTALL_STD_DIR)
	cp -rf extra $(INSTALL_LIB_DIR)
	cp haxe $(INSTALL_LIB_DIR)
	ln -s $(INSTALL_LIB_DIR)/haxe $(INSTALL_BIN_DIR)/haxe
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
	rm -rf $(INSTALL_BIN_DIR)/haxe $(INSTALL_BIN_DIR)/haxelib
	if [ -d "$(INSTALL_LIB_DIR)/lib" ] && find "$(INSTALL_LIB_DIR)/lib" -mindepth 1 -print -quit | grep -q .; then \
		echo "The local haxelib repo at $(INSTALL_LIB_DIR)/lib will not be removed. Remove it manually if you want."; \
		find $(INSTALL_LIB_DIR)/ ! -name 'lib' -mindepth 1 -maxdepth 1 -exec rm -rf {} +; \
	else \
		rm -rf $(INSTALL_LIB_DIR); \
	fi

# Dependencies

-include Makefile.dependencies

# Package

package_src:
	mkdir -p $(PACKAGE_OUT_DIR)
	# use git-archive-all since we have submodules
	# https://github.com/Kentzo/git-archive-all
	curl -s https://raw.githubusercontent.com/Kentzo/git-archive-all/1.15/git_archive_all.py -o extra/git_archive_all.py
	python extra/git_archive_all.py $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_src$(PACKAGE_SRC_EXTENSION)

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
	haxelib git hxparse https://github.com/Simn/hxparse master src
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
	$(foreach lib,$(EXTLIB_LIBS),$(MAKE) -C libs/$(lib) clean &&) true

clean_haxe:
	rm -f -r _build $(OUTPUT)

clean_tools:
	rm -f $(OUTPUT) haxelib

clean_package:
	rm -rf $(PACKAGE_OUT_DIR)

# SUFFIXES

.ml.cmx:
	$(CC_CMD)

.ml.cmo:
	$(CC_CMD)

.PHONY: haxe libs haxelib
