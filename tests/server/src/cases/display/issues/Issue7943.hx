package cases.display.issues;

import haxe.display.Diagnostic;

class Issue7943 extends DisplayTestCase {
	/**
		class Main {
			static function main() {
				{-1-}0{-2-}{-3-}1{-4-};
			}
		}
	**/
	function test(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.equals(1, diags.length);
		Assert.isTrue(diags[0].kind == DKParserError);
		Assert.equals(Error, diags[0].severity);
		Assert.equals("Missing ;", diags[0].args);
		Assert.same(range(3, 4), diags[0].range);
	}
}
