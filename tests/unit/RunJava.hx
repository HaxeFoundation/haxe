class RunJava {

	static function main() {
		var p = new sys.io.Process("java", ["-jar", neko.Web.getCwd() + "/java/java.jar"]);
		if( neko.Web.isModNeko )
			neko.Web.setHeader("Content-Type","text/plain");
		try {
			while( true ) {
				var c = p.stdout.readByte();
				neko.Lib.print(StringTools.htmlEscape(String.fromCharCode(c)));
			}
		} catch ( e : haxe.io.Eof ) {
		}
		neko.Lib.print(StringTools.htmlEscape(p.stderr.readAll().toString()));
	}

}