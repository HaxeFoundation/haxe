package cases.display.issues;

import haxe.display.Diagnostic;

class Issue10194 extends DisplayTestCase {
	/**
		function f(a:Int, b:Int) {}

		function main() {
			var v = 10; // reported as "unused variable"
			f({-1-}""{-2-}, v);
		}
	**/
	function test(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKCompilerError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("String should be Int\nFor function argument 'a'", diags[0].args);
		Assert.same(range(1, 2), diags[0].range);
	}
}
