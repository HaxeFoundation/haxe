@ocamake -a *.ml -o swfLib.cma
@ocamake -a *.ml -o swfLib.cmxa
@cp -f swf.cmi swfParser.cmi swfZip.cmi actionScript.cmi swfLib.lib swfLib.cma swfLib.cmxa c:\ocaml\lib
@ocamake -a *.ml -o swfLib.cma -clean
@ocamake -a *.ml -o swfLib.cmxa -clean
@pause