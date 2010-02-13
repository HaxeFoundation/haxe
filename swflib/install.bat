@ocamake -a *.ml *.mli -o swfLib.cma -clean
@ocamake -a *.ml *.mli -o swfLib.cmxa -clean
@ocamake -a *.ml *.mli -o swfLib.cma
@ocamake -g -a *.ml *.mli -o swfLib.cmxa
@cp -f *.cmi swfLib.lib swfLib.cma swfLib.cmxa c:\ocaml\lib
@ocamake -a *.ml *.mli -o swfLib.cma -clean
@ocamake -a *.ml *.mli -o swfLib.cmxa -clean
@pause
