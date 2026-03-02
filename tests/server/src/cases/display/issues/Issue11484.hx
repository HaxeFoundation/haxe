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
		var diag = diags.find(d -> d.kind == MissingFields);
		Assert.notNull(diag);
		Assert.same(range(1, 2), diag.range);
	}
}
