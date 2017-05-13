#!/bin/sh
if [ ! -f "_build/src/syntax/parser.ml" -o "src/syntax/parser.mly" -nt "_build/src/syntax/parser.ml" ]; then
	camlp4o -impl src/syntax/parser.mly -o _build/src/syntax/parser.ml
fi

if [ "$1" -eq "0" -a ! -f "_build/src/compiler/version.ml" ]; then
	echo let version_extra = None > _build/src/compiler/version.ml
fi