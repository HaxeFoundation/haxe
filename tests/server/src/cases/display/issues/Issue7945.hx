package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7945 extends DisplayTestCase {
	/**
		abstract Test(Int) {-1-}too{-2-} Int {}
	**/
	function test1(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Expected { or to or from", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}

	/**
		class Test {-1-}extend{-2-} OtherClass {}
	**/
	function test2(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Expected extends or implements or {", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
