package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7932 extends DisplayTestCase {
	/**
		class Main< {-1-}{{-2-}
			public static function main() {}
		}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Expected type parameter", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
