package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7944 extends DisplayTestCase {
	/**
		class Main {
		static function main() {}

		{-1-}fun{-2-} f() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unexpected fun", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
