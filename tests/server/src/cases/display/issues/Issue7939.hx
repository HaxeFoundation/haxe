package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7939 extends DisplayTestCase {
	/**
		typedef Struct {-1-}{{-2-}}
	**/
	function test(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unexpected {", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
