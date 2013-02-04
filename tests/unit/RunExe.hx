class RunExe {


static function main() {
		/*
		cpp: cpp/Test-debug
		cs : cs/bin/cs
		cs_unsafe : cs_unsafe/bin/cs_unsafe
		*/
		var rel_path = "";
		if (neko.Web.isModNeko) {
			//mod neko: use get string
			rel_path = neko.Web.getParamsString();
			//display similar to other tests
			neko.Web.setHeader("Content-Type","text/plain");
		} else {
			//command line arg
			var args = Sys.args();
			if (args.length > 0) rel_path = args[0];
		}
		
		if (rel_path == "") {
			neko.Lib.print("error:no path to executable specified");
			return;
		}
		
		var p = new sys.io.Process(neko.Web.getCwd() + rel_path, []);
		
		try {
			while ( true ) {
				var c = p.stdout.readByte();
				neko.Lib.print(StringTools.htmlEscape(String.fromCharCode(c)));
			}
		} catch ( e : haxe.io.Eof ) {

		}
		neko.Lib.print(StringTools.htmlEscape(p.stderr.readAll().toString()));
	}
}
