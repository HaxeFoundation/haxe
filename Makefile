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

MAKEFILENAME?=Makefile
PLATFORM?=unix

OUTPUT=haxe
EXTENSION=
OCAMLOPT?=ocamlopt
OCAMLC?=ocamlc
LFLAGS=
STATICLINK?=0

HAXE_DIRECTORIES=compiler context generators macro optimization syntax typing display

INCLUDES = $(HAXE_DIRECTORIES:%=-I _build/src/%)

CFLAGS= -bin-annot
ALL_CFLAGS= $(CFLAGS) -g -w -3 -I libs/extlib -I libs/extc -I libs/neko -I libs/javalib -I libs/ziplib -I libs/swflib -I libs/xml-light -I libs/ttflib -I libs/ilib -I libs/objsize -I libs/pcre \
	$(INCLUDES)

LIBS=unix str libs/extlib/extLib libs/xml-light/xml-light libs/swflib/swflib \
	libs/extc/extc libs/neko/neko libs/javalib/java libs/ziplib/zip \
	libs/ttflib/ttf libs/ilib/il libs/objsize/objsize libs/pcre/pcre


ifneq ($(STATICLINK),0)
LIB_PARAMS= -cclib '-Wl,-Bstatic -lpcre -lz -Wl,-Bdynamic '

else
LIB_PARAMS?= -cclib -lpcre -cclib -lz

endif

NATIVE_LIBS=-cclib libs/extc/extc_stubs.o -cclib libs/extc/process_stubs.o -cclib libs/objsize/c_objsize.o -cclib libs/pcre/pcre_stubs.o -ccopt -L/usr/local/lib $(LIB_PARAMS)

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

RELDIR=../../..

-include Makefile.modules

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
	make -C libs/pcre OCAMLOPT=$(OCAMLOPT) OCAMLC=$(OCAMLC) $(TARGET_FLAG)

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
	ocamldep -sort -slash $(INCLUDES) $(MODULES) | sed -e "s/\.ml//g" >> Makefile.modules

build_pass_3:
	ocamldep -slash -native $(INCLUDES) $(MODULES:%=%.ml) > Makefile.dependencies

build_pass_4: $(MODULES:%=%.$(MODULE_EXT))
	$(COMPILER) -o $(OUTPUT) $(NATIVE_LIBS) $(NATIVE_LIB_FLAG) $(LFLAGS) $(LIBS:=.$(LIB_EXT)) $(MODULES:%=%.$(MODULE_EXT))

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

# Modules

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
	make -C libs/pcre clean

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