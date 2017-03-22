#!/bin/sh
if [ ! -f "_build/src/syntax/parser.ml" -o "src/syntax/parser.mly" -nt "_build/src/syntax/parser.ml" ]; then
	camlp4o -impl src/syntax/parser.mly -o _build/src/syntax/parser.ml
fi

if [ ! -f "_build/src/syntax/lexer.ml" -o "src/syntax/lexer.mll" -nt "_build/src/syntax/lexer.ml" ]; then
	ocamllex -o _build/src/syntax/lexer.ml src/syntax/lexer.mll
fi

if [ "$1" -eq "0" -a ! -f "_build/src/compiler/version.ml" ]; then
	echo let version_extra = None > _build/src/compiler/version.ml
fi