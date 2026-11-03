@:js.import(@star "../lib.js")
extern class Lib {
	static function greet():String;
}

@:expose("lib.Api")
class Api {
	public static function greet()
		return Lib.greet();
}

class Main {
	static function main() {
		// the output is an ES module, so `$hx_exports` falls back to `globalThis` on Node
		var api:Dynamic = js.Syntax.code("globalThis.lib.Api");
		if (api == null || api.greet() != "hello")
			throw "Api is not exposed";
	}
}
