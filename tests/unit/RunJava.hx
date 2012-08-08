class RunJava {

	static function main() {
		var p = new neko.io.Process("java",["-jar",neko.Web.getCwd()+"/java/java.jar"]);
		try {
			while( true ) {
				var c = p.stdout.readByte();
				if( c == "\n".code )
					neko.Lib.print("<br>");
				else
					neko.Lib.print(StringTools.htmlEscape(String.fromCharCode(c)));
			}
		} catch ( e : haxe.io.Eof ) {
		}
		neko.Lib.print(StringTools.htmlEscape(p.stderr.readAll().toString()).split("\n").join("<br>"));
	}

}