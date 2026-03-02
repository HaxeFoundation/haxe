package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7946 extends DisplayTestCase {
	/**
		{-1-}open{-2-} haxe.Json;
	**/
	function test1(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unexpected open", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}

	/**
		{-1-}clas{-2-} Test {}
	**/
	function test(_) {
		runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		var diags = parseDiagnostics();
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unexpected clas", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
