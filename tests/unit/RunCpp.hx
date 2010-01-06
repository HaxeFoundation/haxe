class RunCpp {

	static function main() {
		var p = new neko.io.Process(neko.Web.getCwd()+"cpp/Test",[]);
		try {
			while( true ) {
				var c = p.stdout.readByte();
				if( c == "\n".code )
					neko.Lib.print("<br>");
				else
					neko.Lib.print(StringTools.htmlEscape(String.fromCharCode(c)));
			}
		} catch( e : haxe.io.Eof ) {
		}
		neko.Lib.print(StringTools.htmlEscape(p.stderr.readAll().toString()).split("\n").join("<br>"));
	}

}