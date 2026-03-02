package cases.display.issues;

import haxe.display.Diagnostic;

class Issue11484 extends DisplayTestCase {
	/**
		class Foo {}

		class Main {
			static function main() {
				{-1-}new Foo(1, "test"){-2-};
			}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		var r = range(1, 2);
		Assert.isTrue(diags.exists(d -> d.kind == MissingFields && d.range == r));
	}
}
