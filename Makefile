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

INSTALL_DIR=/usr/local
INSTALL_BIN_DIR=$(INSTALL_DIR)/bin
INSTALL_LIB_DIR=$(INSTALL_DIR)/lib/haxe
INSTALL_STD_DIR=$(INSTALL_DIR)/share/haxe/std
PACKAGE_OUT_DIR=out
INSTALLER_TMP_DIR=installer
PACKAGE_SRC_EXTENSION=.tar.gz

MAKEFILENAME?=Makefile
PLATFORM?=unix

HAXE_OUTPUT=haxe
HAXELIB_OUTPUT=haxelib
PREBUILD_OUTPUT=prebuild
EXTENSION=
LFLAGS=
STATICLINK?=0

# Configuration

# Modules in these directories should only depend on modules that are in directories to the left
HAXE_DIRECTORIES=core core/ds core/json core/display syntax context context/display codegen codegen/gencommon generators generators/jvm optimization filters macro macro/eval macro/eval/bytes typing compiler
EXTLIB_LIBS=extlib_leftovers extc neko javalib swflib ttflib ilib objsize pcre ziplib
OCAML_LIBS=unix str threads dynlink
OPAM_LIBS=sedlex.ppx xml-light extlib ptmap sha

FINDLIB_LIBS=$(OCAML_LIBS)
FINDLIB_LIBS+=$(OPAM_LIBS)

# Includes, packages and compiler

HAXE_INCLUDES=$(HAXE_DIRECTORIES:%=-I _build/src/%)
EXTLIB_INCLUDES=$(EXTLIB_LIBS:%=-I libs/%)
ALL_INCLUDES=$(EXTLIB_INCLUDES) $(HAXE_INCLUDES)
FINDLIB_PACKAGES=$(FINDLIB_LIBS:%=-package %)
CFLAGS=
ALL_CFLAGS=-bin-annot -safe-string -thread -g -w -3 -w -40 $(CFLAGS) $(ALL_INCLUDES) $(FINDLIB_PACKAGES)

MESSAGE_FILTER=sed -e 's/_build\/src\//src\//' tmp.tmp

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
	OCAMLDEP_FLAGS = -native
endif

CC_CMD = ($(COMPILER) $(ALL_CFLAGS) -c $< 2>tmp.tmp && $(MESSAGE_FILTER)) || ($(MESSAGE_FILTER) && exit 1)

# Meta information

BUILD_DIRECTORIES := $(HAXE_DIRECTORIES:%=_build/src/%)
HAXE_SRC := $(wildcard $(HAXE_DIRECTORIES:%=src/%/*.ml))
BUILD_SRC := $(HAXE_SRC:%=_build/%)

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
HAXE_VERSION=$(shell $(CURDIR)/$(HAXE_OUTPUT) -version 2>&1 | awk '{print $$1;}')
HAXE_VERSION_SHORT=$(shell echo "$(HAXE_VERSION)" | grep -oE "^[0-9]+\.[0-9]+\.[0-9]+")

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

_build/%:%
	mkdir -p $(dir $@)
	cp $< $@

build_dirs:
	@mkdir -p $(BUILD_DIRECTORIES)

_build/src/syntax/grammar.ml:src/syntax/grammar.mly
	camlp5o -impl $< -o $@

_build/src/compiler/version.ml: FORCE
ifneq ($(ADD_REVISION),0)
	$(MAKE) -f Makefile.version_extra -s --no-print-directory ADD_REVISION=$(ADD_REVISION) BRANCH=$(BRANCH) COMMIT_SHA=$(COMMIT_SHA) COMMIT_DATE=$(COMMIT_DATE) > _build/src/compiler/version.ml
else
	echo let version_extra = None > _build/src/compiler/version.ml
endif

haxe:
	dune build --workspace dune-workspace.dev src-prebuild/prebuild.exe
	_build/default/src-prebuild/prebuild.exe libparams $(LIB_PARAMS) > lib.sexp
	dune build --workspace dune-workspace.dev src/haxe.exe
	cp -f _build/default/src/haxe.exe ./${HAXE_OUTPUT}

kill_exe_win:
ifdef SYSTEMROOT
	-@taskkill /F /IM haxe.exe 2>/dev/null
endif

plugin:
ifeq ($(BYTECODE),1)
	$(CC_CMD) $(PLUGIN).ml
else
	$(COMPILER) $(ALL_CFLAGS) -shared -o $(PLUGIN).cmxs $(PLUGIN).ml
endif

# Only use if you have only changed gencpp.ml
quickcpp: build_src build_pass_4 copy_haxetoolkit

CPP_OS := $(shell uname)
ifeq ($(CPP_OS),Linux)
copy_haxetoolkit:
	sudo cp haxe /usr/bin/haxe
else
copy_haxetoolkit: /cygdrive/c/HaxeToolkit/haxe/haxe.exe
/cygdrive/c/HaxeToolkit/haxe/haxe.exe:haxe.exe
	cp $< $@
endif

haxelib:
	(cd $(CURDIR)/extra/haxelib_src && $(CURDIR)/$(HAXE_OUTPUT) client.hxml && nekotools boot run.n)
	mv extra/haxelib_src/run$(EXTENSION) $(HAXELIB_OUTPUT)

tools: haxelib

install: uninstall
	mkdir -p "$(DESTDIR)$(INSTALL_BIN_DIR)"
	cp $(HAXE_OUTPUT) $(HAXELIB_OUTPUT) "$(DESTDIR)$(INSTALL_BIN_DIR)"
	mkdir -p "$(DESTDIR)$(INSTALL_STD_DIR)"
	cp -r std/* "$(DESTDIR)$(INSTALL_STD_DIR)"

uninstall:
	rm -rf $(DESTDIR)$(INSTALL_BIN_DIR)/$(HAXE_OUTPUT) $(DESTDIR)$(INSTALL_BIN_DIR)/$(HAXELIB_OUTPUT)
	if [ -d "$(DESTDIR)$(INSTALL_LIB_DIR)/lib" ] && find "$(DESTDIR)$(INSTALL_LIB_DIR)/lib" -mindepth 1 -print -quit | grep -q .; then \
		echo "The local haxelib repo at $(DESTDIR)$(INSTALL_LIB_DIR)/lib will not be removed. Remove it manually if you want."; \
		find $(DESTDIR)$(INSTALL_LIB_DIR)/ ! -name 'lib' -mindepth 1 -maxdepth 1 -exec rm -rf {} +; \
	else \
		rm -rf $(DESTDIR)$(INSTALL_LIB_DIR); \
	fi
	rm -rf $(DESTDIR)$(INSTALL_STD_DIR)

opam_install:
	opam install $(OPAM_LIBS) camlp5 ocamlfind --yes

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
	cp -r $(HAXE_OUTPUT) $(HAXELIB_OUTPUT) std extra/LICENSE.txt extra/CONTRIB.txt extra/CHANGES.txt $(PACKAGE_FILE_NAME)
	# archive
	tar -zcf $(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_bin.tar.gz $(PACKAGE_FILE_NAME)
	rm -r $(PACKAGE_FILE_NAME)

package_bin: package_$(PLATFORM)

xmldoc:
	cd extra && \
	$(CURDIR)/$(HAXELIB_OUTPUT) newrepo && \
	$(CURDIR)/$(HAXELIB_OUTPUT) git hxcpp  https://github.com/HaxeFoundation/hxcpp   && \
	$(CURDIR)/$(HAXELIB_OUTPUT) git hxjava https://github.com/HaxeFoundation/hxjava  && \
	$(CURDIR)/$(HAXELIB_OUTPUT) git hxcs   https://github.com/HaxeFoundation/hxcs    && \
	PATH="$(CURDIR):$(PATH)" $(CURDIR)/$(HAXE_OUTPUT) doc.hxml

$(INSTALLER_TMP_DIR):
	mkdir -p $(INSTALLER_TMP_DIR)

$(INSTALLER_TMP_DIR)/neko-osx64.tar.gz: $(INSTALLER_TMP_DIR)
	wget -nv http://nekovm.org/media/neko-2.1.0-osx64.tar.gz -O installer/neko-osx64.tar.gz

# Installer

package_installer_mac: $(INSTALLER_TMP_DIR)/neko-osx64.tar.gz package_unix
	$(eval OUTFILE := $(shell pwd)/$(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_installer.tar.gz)
	$(eval PACKFILE := $(shell pwd)/$(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_bin.tar.gz)
	$(eval VERSION := $(shell $(CURDIR)/$(HAXE_OUTPUT) -version 2>&1))
	$(eval NEKOVER := $(shell neko -version 2>&1))
	bash -c "rm -rf $(INSTALLER_TMP_DIR)/{resources,pkg,tgz,haxe.tar.gz}"
	mkdir $(INSTALLER_TMP_DIR)/resources
	# neko - unpack to change the dir name
	cd $(INSTALLER_TMP_DIR)/resources && tar -zxvf ../neko-osx64.tar.gz
	mv $(INSTALLER_TMP_DIR)/resources/neko* $(INSTALLER_TMP_DIR)/resources/neko
	cd $(INSTALLER_TMP_DIR)/resources && tar -zcvf neko.tar.gz neko
	# haxe - unpack to change the dir name
	cd $(INSTALLER_TMP_DIR)/resources && tar -zxvf $(PACKFILE)
	mv $(INSTALLER_TMP_DIR)/resources/haxe* $(INSTALLER_TMP_DIR)/resources/haxe
	cd $(INSTALLER_TMP_DIR)/resources && tar -zcvf haxe.tar.gz haxe
	# scripts
	cp -rf extra/mac-installer/* $(INSTALLER_TMP_DIR)/resources
	cd $(INSTALLER_TMP_DIR)/resources && tar -zcvf scripts.tar.gz scripts
	# installer structure
	mkdir -p $(INSTALLER_TMP_DIR)/pkg
	cd $(INSTALLER_TMP_DIR)/pkg && xar -xf ../resources/installer-structure.pkg .
	mkdir $(INSTALLER_TMP_DIR)/tgz; mv $(INSTALLER_TMP_DIR)/resources/*.tar.gz $(INSTALLER_TMP_DIR)/tgz
	cd $(INSTALLER_TMP_DIR)/tgz; find . | cpio -o --format odc | gzip -c > ../pkg/files.pkg/Payload
	cd $(INSTALLER_TMP_DIR)/pkg/files.pkg && bash -c "INSTKB=$$(du -sk ../../tgz | awk '{print $$1;}'); \
	du -sk ../../tgz; \
	echo $$INSTKB ; \
	INSTKBH=`expr $$INSTKB - 4`; \
	echo $$INSTKBH ;\
	sed -i '' 's/%%INSTKB%%/$$INSTKBH/g' PackageInfo ;\
	sed -i '' 's/%%VERSION%%/$(VERSION)/g' PackageInfo ;\
	sed -i '' 's/%%VERSTRING%%/$(VERSION)/g' PackageInfo ;\
	sed -i '' 's/%%VERLONG%%/$(VERSION)/g' PackageInfo ;\
	sed -i '' 's/%%NEKOVER%%/$(NEKOVER)/g' PackageInfo ;\
	cd .. ;\
	sed -i '' 's/%%VERSION%%/$(VERSION)/g' Distribution ;\
	sed -i '' 's/%%VERSTRING%%/$(VERSION)/g' Distribution ;\
	sed -i '' 's/%%VERLONG%%/$(VERSION)/g' Distribution ;\
	sed -i '' 's/%%NEKOVER%%/$(NEKOVER)/g' Distribution ;\
	sed -i '' 's/%%INSTKB%%/$$INSTKBH/g' Distribution"
	# repackage
	cd $(INSTALLER_TMP_DIR)/pkg; xar --compression none -cf ../$(PACKAGE_FILE_NAME).pkg *
	# tar
	cd $(INSTALLER_TMP_DIR); tar -zcvf $(OUTFILE) $(PACKAGE_FILE_NAME).pkg

# Clean

clean: clean_libs clean_haxe clean_tools clean_package

clean_libs:
	$(foreach lib,$(EXTLIB_LIBS),$(MAKE) -C libs/$(lib) clean &&) true

clean_haxe:
	rm -f -r _build $(HAXE_OUTPUT) $(PREBUILD_OUTPUT)

clean_tools:
	rm -f $(HAXE_OUTPUT) $(PREBUILD_OUTPUT) $(HAXELIB_OUTPUT)

clean_package:
	rm -rf $(PACKAGE_OUT_DIR)

FORCE:

# SUFFIXES

.ml.cmx:
	$(CC_CMD)

.ml.cmo:
	$(CC_CMD)

.PHONY: haxe libs haxelib
