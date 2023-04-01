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

DUNE_COMMAND=dune
HAXE_OUTPUT=haxe
HAXELIB_OUTPUT=haxelib
PREBUILD_OUTPUT=prebuild
EXTENSION=
LFLAGS=
STATICLINK?=0

SYSTEM_NAME=Unknown
ifeq ($(OS),Windows_NT)
	SYSTEM_NAME=Windows
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		SYSTEM_NAME=Linux
	endif
	ifeq ($(UNAME_S),Darwin)
		SYSTEM_NAME=Mac
	endif
endif

# Configuration

ADD_REVISION?=0

BRANCH=$(shell git rev-parse --abbrev-ref HEAD)
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

NEKO_VERSION=2.3.0
NEKO_MAJOR_VERSION=$(shell echo "$(NEKO_VERSION)" | grep -oE "^[0-9]+")
NEKO_VERSION_TAG=v$(shell echo "$(NEKO_VERSION)" | sed "s/\./-/g")

ifneq ($(STATICLINK),0)
	LIB_PARAMS= -cclib '-Wl,-Bstatic -lpcre2-8 -lz -lmbedtls -lmbedx509 -lmbedcrypto -Wl,-Bdynamic '
else
	LIB_PARAMS?= -cclib -lpcre2-8 -cclib -lz -cclib -lmbedtls -cclib -lmbedx509 -cclib -lmbedcrypto
endif
ifeq ($(SYSTEM_NAME),Mac)
	LIB_PARAMS+= -cclib '-framework Security -framework CoreFoundation'
endif

all: haxe tools

haxe:
	$(DUNE_COMMAND) build --workspace dune-workspace.dev src-prebuild/prebuild.exe
	_build/default/src-prebuild/prebuild.exe libparams $(LIB_PARAMS) > lib.sexp
	_build/default/src-prebuild/prebuild.exe version "$(ADD_REVISION)" "$(BRANCH)" "$(COMMIT_SHA)" > src/compiler/version.ml
	$(DUNE_COMMAND) build --workspace dune-workspace.dev src/haxe.exe
	cp -f _build/default/src/haxe.exe ./"$(HAXE_OUTPUT)"

plugin: haxe
	$(DUNE_COMMAND) build --workspace dune-workspace.dev plugins/$(PLUGIN)/$(PLUGIN).cmxs
	mkdir -p plugins/$(PLUGIN)/cmxs/$(SYSTEM_NAME)
	cp -f _build/default/plugins/$(PLUGIN)/$(PLUGIN).cmxs plugins/$(PLUGIN)/cmxs/$(SYSTEM_NAME)/plugin.cmxs

kill_exe_win:
ifdef SYSTEMROOT
	-@taskkill /F /IM haxe.exe 2>/dev/null
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

# haxelib should depends on haxe, but we don't want to do that...
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
	opam install camlp5 ocamlfind dune --yes

haxe_deps:
	opam pin add haxe . --no-action
	opam install haxe --deps-only --yes

# Package

package_env: opam_install haxe_deps

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
	wget -nv https://github.com/HaxeFoundation/neko/releases/download/$(NEKO_VERSION_TAG)/neko-$(NEKO_VERSION)-osx64.tar.gz -O installer/neko-osx64.tar.gz

# Installer

package_installer_mac: $(INSTALLER_TMP_DIR)/neko-osx64.tar.gz package_unix
	$(eval OUTFILE := $(shell pwd)/$(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_installer.tar.gz)
	$(eval PACKFILE := $(shell pwd)/$(PACKAGE_OUT_DIR)/$(PACKAGE_FILE_NAME)_bin.tar.gz)
	$(eval VERSION := $(shell $(CURDIR)/$(HAXE_OUTPUT) -version 2>&1))
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
	sed -i '' 's/%%NEKO_VERSION%%/$(NEKO_VERSION)/g' $(INSTALLER_TMP_DIR)/resources/scripts/neko-postinstall.sh
	sed -i '' 's/%%NEKO_MAJOR_VERSION%%/$(NEKO_MAJOR_VERSION)/g' $(INSTALLER_TMP_DIR)/resources/scripts/neko-postinstall.sh
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
	sed -i '' 's/%%NEKOVER%%/$(NEKO_VERSION)/g' PackageInfo ;\
	cd .. ;\
	sed -i '' 's/%%VERSION%%/$(VERSION)/g' Distribution ;\
	sed -i '' 's/%%VERSTRING%%/$(VERSION)/g' Distribution ;\
	sed -i '' 's/%%VERLONG%%/$(VERSION)/g' Distribution ;\
	sed -i '' 's/%%NEKOVER%%/$(NEKO_VERSION)/g' Distribution ;\
	sed -i '' 's/%%INSTKB%%/$$INSTKBH/g' Distribution"
	# repackage
	cd $(INSTALLER_TMP_DIR)/pkg; xar --compression none -cf ../$(PACKAGE_FILE_NAME).pkg *
	# tar
	cd $(INSTALLER_TMP_DIR); tar -zcvf $(OUTFILE) $(PACKAGE_FILE_NAME).pkg

# Clean

clean: clean_haxe clean_tools clean_package

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

.PHONY: haxe haxelib

# our "all:" target doens't work in parallel mode
.NOTPARALLEL:
