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

OUTPUT=haxe
EXTENSION=
OCAMLOPT=ocamlopt

CFLAGS= -g -I libs/extlib -I libs/extc -I libs/neko -I libs/javalib -I libs/ziplib -I libs/swflib -I libs/xml-light

CC_CMD = $(OCAMLOPT) $(CFLAGS) -c $<
CC_PARSER_CMD = $(OCAMLOPT) -pp camlp4o $(CFLAGS) -c parser.ml

LIBS=unix.cmxa str.cmxa libs/extlib/extLib.cmxa libs/xml-light/xml-light.cmxa libs/swflib/swflib.cmxa \
	libs/extc/extc.cmxa libs/neko/neko.cmxa libs/javalib/java.cmxa libs/ziplib/zip.cmxa

NATIVE_LIBS=-cclib libs/extc/extc_stubs.o -cclib -lz

RELDIR=../../..

EXPORT=../../../projects/motionTools/haxe

MODULES=ast type lexer common genxml parser typecore optimizer typeload \
codegen gencommon genas3 gencpp genjs genneko genphp genswf8 \
	genswf9 genswf genjava gencs interp typer matcher dce main

HAXE_LIBRARY_PATH=$(CURDIR)/std

all: libs haxe tools

libs:
	make -C libs/extlib opt
	make -C libs/extc native
	make -C libs/neko
	make -C libs/javalib
	make -C libs/ziplib
	make -C libs/swflib
	make -C libs/xml-light xml-light.cmxa

haxe: $(MODULES:=.cmx)
	$(OCAMLOPT) -o $(OUTPUT) $(NATIVE_LIBS) $(LIBS) $(MODULES:=.cmx)

haxelib:
	$(CURDIR)/$(OUTPUT) --cwd "$(CURDIR)/std/tools/haxelib" haxelib.hxml
	cp std/tools/haxelib/haxelib$(EXTENSION) haxelib$(EXTENSION)

haxedoc:
	$(CURDIR)/$(OUTPUT) --cwd "$(CURDIR)/std/tools/haxedoc" haxedoc.hxml
	cp std/tools/haxedoc/haxedoc$(EXTENSION) haxedoc$(EXTENSION)

tools: haxelib haxedoc

export:
	cp haxe*.exe doc/CHANGES.txt $(EXPORT)
	rsync -a --exclude .svn --exclude *.n --exclude std/libs --delete std $(EXPORT)

codegen.cmx: typeload.cmx typecore.cmx type.cmx genxml.cmx common.cmx ast.cmx

common.cmx: type.cmx ast.cmx

dce.cmx: ast.cmx common.cmx type.cmx

genas3.cmx: type.cmx common.cmx codegen.cmx ast.cmx

gencommon.cmx: type.cmx common.cmx codegen.cmx ast.cmx

gencpp.cmx: type.cmx lexer.cmx common.cmx codegen.cmx ast.cmx

gencs.cmx: type.cmx lexer.cmx gencommon.cmx common.cmx codegen.cmx ast.cmx

genjava.cmx: type.cmx gencommon.cmx common.cmx codegen.cmx ast.cmx

genjs.cmx: type.cmx optimizer.cmx lexer.cmx common.cmx codegen.cmx ast.cmx

genneko.cmx: type.cmx lexer.cmx common.cmx codegen.cmx ast.cmx

genphp.cmx: type.cmx lexer.cmx common.cmx codegen.cmx ast.cmx

genswf.cmx: type.cmx genswf9.cmx genswf8.cmx common.cmx ast.cmx

genswf8.cmx: type.cmx lexer.cmx common.cmx codegen.cmx ast.cmx

genswf9.cmx: type.cmx lexer.cmx genswf8.cmx common.cmx codegen.cmx ast.cmx

genxml.cmx: type.cmx lexer.cmx common.cmx ast.cmx

interp.cmx: typecore.cmx type.cmx lexer.cmx genneko.cmx common.cmx codegen.cmx ast.cmx genswf.cmx parser.cmx

matcher.cmx: codegen.cmx typecore.cmx type.cmx typer.cmx common.cmx ast.cmx

main.cmx: dce.cmx matcher.cmx typer.cmx typeload.cmx typecore.cmx type.cmx parser.cmx optimizer.cmx lexer.cmx interp.cmx genxml.cmx genswf.cmx genphp.cmx genneko.cmx genjs.cmx gencpp.cmx genas3.cmx common.cmx codegen.cmx ast.cmx gencommon.cmx

optimizer.cmx: typecore.cmx type.cmx parser.cmx common.cmx ast.cmx

parser.cmx: parser.ml lexer.cmx common.cmx ast.cmx
	$(CC_PARSER_CMD)

type.cmx: ast.cmx

typecore.cmx: type.cmx common.cmx ast.cmx

typeload.cmx: typecore.cmx type.cmx parser.cmx optimizer.cmx lexer.cmx common.cmx ast.cmx

typer.cmx: typeload.cmx typecore.cmx type.cmx parser.cmx optimizer.cmx lexer.cmx interp.cmx genneko.cmx genjs.cmx common.cmx codegen.cmx ast.cmx

lexer.cmx: lexer.ml

lexer.cmx: ast.cmx


clean: clean_libs clean_haxe clean_tools

clean_libs:
	make -C libs/extlib clean
	make -C libs/extc clean
	make -C libs/neko clean
	make -C libs/ziplib clean
	make -C libs/javalib clean
	make -C libs/swflib clean
	make -C libs/xml-light clean

clean_haxe:
	rm -f $(MODULES:=.obj) $(MODULES:=.o) $(MODULES:=.cmx) $(MODULES:=.cmi) lexer.ml

clean_tools:
	rm -f $(OUTPUT) haxelib haxedoc

# SUFFIXES
.ml.cmx:
	$(CC_CMD)

.mli.cmi:
	$(CC_CMD)

.mll.ml:
	ocamllex $<

.PHONY: haxe libs
