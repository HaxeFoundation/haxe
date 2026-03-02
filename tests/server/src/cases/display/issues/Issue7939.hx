package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7939 extends DisplayTestCase {
	/**
		typedef Struct {-1-}{{-2-}}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unexpected {", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
