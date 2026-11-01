package cases.display.issues;

import haxe.display.Diagnostic;

class Issue5306 extends DisplayTestCase {
	/**
		{-1-}import {-3-}InvalidType{-4-};{-2-}

		class Main {
			static function main() {
				var ib:Array<Int>;
				{-5-}ib{-6-}[0] = 0; ib[1] = 1; ib[2]
				{-7-}trace{-8-}("test");
			}
		}
	**/
	function test(_) {
		var files = runHaxeJson([], DisplayMethods.Diagnostics, {file: file});
		final diags:Array<haxe.display.Diagnostic<Any>> = files != null && files.length > 0 ? cast files[0].diagnostics : [];
		Assert.isTrue(diags.exists(d -> d.kind == DKParserError && d.args == "Missing ;"));
		Assert.isTrue(diags.exists(d -> d.kind == DKCompilerError && (d.args:String).indexOf("InvalidType") != -1));
		Assert.isTrue(diags.exists(d -> d.kind == DKCompilerError && (d.args:String).indexOf("ib") != -1));
	}
}
