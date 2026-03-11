package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7938 extends DisplayTestCase {
	/**
		class Main {
			public static {-1-}fuction{-2-} main() {}
		}
	**/
	function test(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Unexpected fuction", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
