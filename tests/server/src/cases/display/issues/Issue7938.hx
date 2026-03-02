package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7938 extends DisplayTestCase {
	/**
		class Main {
			public static {-1-}fuction{-2-} main() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unexpected fuction", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
