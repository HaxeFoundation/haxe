package cases.display.issues;

import haxe.display.Diagnostic;

class Issue10713 extends DisplayTestCase {
	/**
		class Main {
		static function main() {
			loadEverything(() -> {
				{-1-}foo{-2-} = 1;
			});
		}

		static function loadEverything(cb:() -> Void, ?what):Void {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		var r = range(1, 2);
		Assert.isTrue(diags.exists(d -> d.kind == MissingFields && Std.string(d.range) == Std.string(r)));
	}
}
